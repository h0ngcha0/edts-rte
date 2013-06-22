%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% This file is part of EDTS.
%%%
%%% EDTS is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% EDTS is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public License
%%% along with EDTS. If not, see <http://www.gnu.org/licenses/>.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This server talks to the "rte interpreter server" to retrieve the binding
%% information. It keeps track of all the neccessary information for displaying
%% the function bodies with temp variables replaced.
%% It also keeps track of the record definition using an ets table, much like
%% what the shell does.

%%%_* Module declaration =======================================================
-module(edts_rte_server).

-behaviour(gen_server).

%%%_* Exports =================================================================

%% server API
-export([start/0, stop/0, start_link/0]).

-export([started_p/0]).

%% Debugger API
-export([ rte_run/3
        , finished_attach/1
        , send_binding/1
        , send_exit/0
        , read_and_add_records/1
        , record_table_name/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Defines ==================================================================
-define(SERVER, ?MODULE).

-record(mfa_info, { mfad          :: {m(), f(), a(), non_neg_integer()}
                  , fun_form      :: term()  %% FIXME type
                  , clauses_lines :: term()  %% FIXME type
                  , line          :: non_neg_integer()
                  , bindings      :: binding()
                  }).

-record(dbg_state, { proc         = unattached :: unattached | pid()
                   , bindings     = []         :: binding()
                   , mfa          = undefined  :: undefined | {m(), f(), a()}
                   , record_table = undefined  :: atom()
                   , depth        = 0          :: non_neg_integer()
                   , line         = undefined  :: undefined | non_neg_integer()
                   , mfa_info     = []         :: [mfa_info()]
                   , subfuns      = []         :: list()  %% FIXME type
                   , module_cache = []         :: list()  %% FIXME type
                   }).

%%%_* Types ====================================================================
-type state()    :: #dbg_state{}.
-type binding()  :: [{atom(), any()}].
-type m()        :: atom().
-type f()        :: atom().
-type a()        :: atom().
-type mfa_info() :: #mfa_info{}.

-export_type([ {binding, 0}
             ]).

%%%_* API ======================================================================
start() ->
  ?MODULE:start_link(),
  {node(), ok}.

stop() ->
  ok.

started_p() -> whereis(?SERVER) =/= undefined.

%%------------------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
%%-----------------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% @doc
%% Run function
%% @end
-spec rte_run(Module::module(), Fun::function(), Args::list()) -> any().
%%------------------------------------------------------------------------------
rte_run(Module, Fun, Args) ->
  gen_server:call(?SERVER, {rte_run, Module, Fun, Args}).

finished_attach(Pid) ->
  gen_server:cast(?SERVER, {finished_attach, Pid}).

send_binding(Msg) ->
  gen_server:cast(?SERVER, {send_binding, Msg}).

send_exit() ->
  gen_server:cast(?SERVER, exit).

%% FIXME: need to come up with a way to add all existing records from
%%        a project and remove records when recompile a particular module
read_and_add_records(Module) ->
  edts_rte_erlang:read_and_add_records(Module, record_table_name()).

record_table_name() ->
  edts_rte_record_table.

%%%_* gen_server callbacks  ====================================================
%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
-spec init(list()) -> {ok, state()} |
                      {ok, state(), timeout()} |
                      ignore |
                      {stop, atom()}.
%%------------------------------------------------------------------------------
init([]) ->
  %% start the int listener. If rte server dies, the int listner will die as
  %% well. this is good because we will have a clean state to start with again.
  edts_rte_int_listener:start(),

  %% records in erlang are purely syntactic sugar. create a table to store the
  %% mapping between records and their definitions.
  %% set the table to public to make debugging easier
  RcdTbl = ets:new(record_table_name(), [public, named_table]),
  {ok, #dbg_state{record_table = RcdTbl}}.

handle_call({rte_run, Module, Fun, Args0}, _From, State) ->
  RcdTbl   = State#dbg_state.record_table,
  %% try to read the record from the current module.. right now this is the
  %% only record support
  AddedRds = edts_rte_erlang:read_and_add_records(Module, RcdTbl),
  io:format("AddedRds:~p~n", [AddedRds]),
  Args     = binary_to_list(Args0),
  ArgsTerm = edts_rte_erlang:convert_list_to_term(Args, RcdTbl),
  io:format("ArgsTerm:~p~n", [ArgsTerm]),
  [Module] = edts_rte_int_listener:interpret_modules([Module]),
  Arity    = length(ArgsTerm),
  io:format("Arity:~p~n", [Arity]),
  {ok, set, {Module, Fun, Arity}} =
    edts_rte_int_listener:set_breakpoint(Module, Fun, Arity),
  io:format("rte_run: after setbreakpoint~n"),
  Pid      = erlang:spawn(Module, Fun, ArgsTerm),
  io:format("called function pid:~p~n", [Pid]),
  {reply, {okn, finished}, State#dbg_state{ proc         = Pid
                                          , bindings     = []
                                          , mfa          = {Module, Fun, Arity}
                                          , line         = undefined
                                          , depth        = 0
                                          , mfa_info     = []
                                          , subfuns      = []
                                          , module_cache = []
                                          }}.

%%------------------------------------------------------------------------------
%% @private
%% @doc Handling all non call/cast messages
%% @end
%%
-spec handle_info(term(), state()) -> {noreply, state()} |
                                      {noreply, state(), Timeout::timeout()} |
                                      {stop, Reason::atom(), state()}.
handle_info(_Msg, State) ->
  %% io:format("rte_server handle_info ...., Msg:~p~n", [Msg]),
  {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
-spec handle_cast(Msg::term(), state()) -> {noreply, state()} |
                                           {noreply, state(), timeout()} |
                                           {stop, Reason::atom(), state()}.
%%------------------------------------------------------------------------------
handle_cast({finished_attach, Pid}, State) ->
  Pid = State#dbg_state.proc,
  edts_rte_int_listener:step(),
  io:format("finish attach.....~n"),
  {noreply, State};
handle_cast({send_binding, {break_at, Bindings, Module, Line, Depth}},State0) ->
  {MFA, State} = get_mfa(State0, Module, Line),
  io:format("1) send_binding......before step. old depth:~p , new_depth:~p~n"
           , [State#dbg_state.depth, Depth]),
  io:format("2) send_binding......Line:~p, Bindings:~p~n",[Line, Bindings]),

  %%io:format("mfa info........... before ~p~n", [State#dbg_state.mfa_info]),

  io:format("3) old mfa:~p~n", [State#dbg_state.mfa]),
  io:format("4) new mfa:~p~n", [MFA]),

  %% get mfa and add one level if it is not main function
  %% output sub function body when the process leaves it.
  %% Only step into one more depth right now.
  {SubFuns, MFAInfo1} =
    case State#dbg_state.depth > Depth of
      true  ->
        io:format( "in send_replacedfun...:~n mfa:~p~nbinding:~p~nDepth:~p~n"
                   , [State#dbg_state.mfa, State#dbg_state.bindings, Depth]),

        %% Pop function up till function's depth <= Depth
        {OutputMFAInfo, RestMFAInfo} =
          lists:splitwith(fun(MFAInfoS) ->
                              {_M, _F, _A, D} = MFAInfoS#mfa_info.mfad,
                              D > Depth
                          end, State#dbg_state.mfa_info),
        io:format("4) OutputMFAInfo:~p~n", [OutputMFAInfo]),
        io:format("5) RestMFAInfo:~p~n", [RestMFAInfo]),
        %% output function bodies
        OutputFuns = output_subfuns(OutputMFAInfo) ++ State#dbg_state.subfuns,
        {OutputFuns, RestMFAInfo};
      false ->
        {State#dbg_state.subfuns, State#dbg_state.mfa_info}
    end,

  MFAInfo = update_mfa_info( MFAInfo1, MFA, Depth, Line, Bindings),
  edts_rte_int_listener:step(),

  %% save current bindings for further use
  {noreply, State#dbg_state{ bindings = Bindings, mfa = MFA
                           , depth = Depth, line = Line
                           , mfa_info = MFAInfo
                           , subfuns = SubFuns}};
handle_cast(exit, #dbg_state{bindings = _Bindings, line = _Line} = State) ->
  %% io:format( "in exit...:~n mfa:~p~nbinding:~p~n"
  %%          , [State#dbg_state.mfa, Bindings]),
  {M, F, A} = State#dbg_state.mfa,

  AllFuns = output_subfuns(State#dbg_state.mfa_info) ++ State#dbg_state.subfuns,
  AllFunsStr = concat_sub_funs(AllFuns),

  %% Fs = make_comments(M, F, A) ++
  %%      ReplacedFun            ++ "\n" ++
  %%      SubFuns,
  io:format("all functions:~p~n", [AllFunsStr]),
  %% io:format( "...............all mfa info keys:~p~n"
  %%          , [dict:fetch_keys(MFAInfo)]),
  send_fun(M, F, A, AllFunsStr),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

output_subfuns(MFAInfos) ->
  lists:foldl(
    fun(MFAInfo0, Funs) ->
           {M, F, A, D} = MFAInfo0#mfa_info.mfad,
           MFA = {M, F, A},
           ReplacedFun = replace_var_in_fun_body(MFA
                                                , D
                                                , MFAInfo0),
           %%io:format("what replacedfunc:~p~n", [ReplacedFun]),
           [{M, F, A, ReplacedFun} | Funs]
          end, [], MFAInfos).


%%------------------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
-spec terminate(Reason::atom(), _State :: state()) -> any().
%%------------------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
-spec code_change(OldVsn::string(), state(), Extra::term()) -> {ok, state()}.
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%_* Internal =================================================================
%% @doc Calculate the MFA based on the line number in the module name. If the
%%      depth is not changed, it is assumed that we remain in the same function
%%      as before, therefore there is no need to re-calculate.
%%      NOTE:
%%      There seems to be a problem with int module. If a function call is
%%      involved in the last expression of a function, when the debugger
%%      process step into the function call, the depth is not changed.
%%
%%      One way to work around this is to cache all the sorted function
%%      info for a particular module and do not use the change of the depth
%%      as the indicator that a new mfa should be calculated. It is too
%%      expensive if no such caching is performed,
get_mfa(State, Module, Line) ->
  case orddict:find(Module, State#dbg_state.module_cache) of
    error ->
      ModFunInfo  = edts_rte_erlang:get_module_sorted_fun_info(Module),
      NewModCache = orddict:store( Module, ModFunInfo
                                 , State#dbg_state.module_cache),
      [_L, F, A]  = find_function(Line, ModFunInfo),
      {{Module, F, A}, State#dbg_state{module_cache = NewModCache}};
    {ok, ModFunInfo} ->
      [_L, F, A]  = find_function(Line, ModFunInfo),
      {{Module, F, A}, State}
  end.

find_function(_L, [])                      ->
  [];
find_function(L, [[L0, _F, _A] = LFA | T]) ->
  case L >= L0 of
    true  -> LFA;
    false -> find_function(L, T)
  end.

replace_var_in_fun_body(MFA, Depth, MFAInfoS) ->
  #mfa_info{ mfad          = Key
           , bindings      = Bindings
           , fun_form      = FunAbsForm
           , clauses_lines = AllClausesLn} = MFAInfoS,
  %% assert
  Key = erlang:append_element(MFA, Depth),
  edts_rte_erlang:var_to_val_in_fun(FunAbsForm, AllClausesLn, Bindings).

concat_sub_funs(SubFuns) ->
  lists:foldl(
    fun({M, F, A, Fun}, FunsBody) ->
        make_comments(M, F, A)  ++
        Fun                     ++ "\n" ++
        FunsBody                ++ "\n"
    end, [], SubFuns).

make_comments(M, F, A) ->
  lists:flatten(io_lib:format("%% ========== Generated by RTE ==========~n"
                              "%% ========== MFA: {~p, ~p, ~p}:~n", [M, F, A])).

send_fun(M, F, Arity, FunBody) ->
  io:format("final function body is:~p~n", [FunBody]),
  lists:foreach(fun(Fun) ->
                    Fun(M, F, Arity, FunBody)
                end, [ fun send_fun_to_edts_web/4
                     , fun send_fun_to_emacs/4
                     ]).

send_fun_to_emacs(M, F, Arity, FunBody) ->
  Id = lists:flatten(io_lib:format("*~p__~p__~p*", [M, F, Arity])),
  io:format("~n~nFunBody:~p~n", [FunBody]),
  Cmd = make_emacsclient_cmd(Id, FunBody),
  io:format("~n~ncmd:~p~n~n~n", [Cmd]),
  os:cmd(Cmd).

make_emacsclient_cmd(Id, FunBody) ->
  lists:flatten(io_lib:format(
                  "emacsclient -e '(edts-display-erl-fun-in-emacs "
                  ++ "~p" ++ " "++"~p" ++" )'", [FunBody, Id])).

send_fun_to_edts_web(M, F, Arity, FunBody) ->
  Id  = lists:flatten(io_lib:format("~p__~p__~p", [M, F, Arity])),
  httpc:request(post, { url(), [], content_type()
                      , mk_editor(Id, FunBody)}, [], []).

url() ->
  "http://localhost:4587/rte/editors/".

content_type() ->
  "application/json".

mk_editor(Id, FunBody) ->
 lists:flatten( io_lib:format(
                  "{\"x\":74,\"y\":92,\"z\":1,\"id\":~p,\"code\":~p}"
               , [Id, FunBody])).

%% @doc if the clauses are unfortunately programmed in the same line
%% then rte shall feel confused and refuse to display any value of
%% the variables.
update_mfa_info( MFAInfo, {M, F, A}, Depth, Line, Bindings) ->
  %% if is tailrecursion OR mfad not the same
  %%    add new mfainfo
  %% else
  %%    update mfainfo
  Key = {M, F, A, Depth},
  io:format("6) params:~p~n", [[MFAInfo, Key, Depth, Line]]),
  AddNewMFAInfoP = add_new_mfa_info_p(MFAInfo, Key, Depth, Line),

  io:format("7) app_p.......:~p~n", [AddNewMFAInfoP]),
  case AddNewMFAInfoP of
    true ->
      %% add new mfa_info
      {ok, FunAbsForm} = edts_code:get_function_body(M, F, A),
      AllClausesLn0    = edts_rte_erlang:extract_fun_clauses_line_num(
                           FunAbsForm),
      AllClausesLn     = edts_rte_erlang:traverse_clause_struct(
                           Line, AllClausesLn0),
      Val = #mfa_info{ mfad          = Key
                     , line          = Line
                     , fun_form      = FunAbsForm
                     , clauses_lines = AllClausesLn
                     , bindings      = Bindings},
      [Val | MFAInfo];
    false ->
      {ok, Val0} = get_hd(MFAInfo),
      #mfa_info{ mfad          = Key
               , clauses_lines = AllClausesLn} = Val0,
      TraversedLns = edts_rte_erlang:traverse_clause_struct(Line, AllClausesLn),
      Val = Val0#mfa_info{ clauses_lines = TraversedLns
                         , line          = Line
                         , bindings      = Bindings
                         },
      [Val | tl(MFAInfo)]
  end.

%% @doc Return true when a new mfa info needs to be added. This will happen
%%      when:
%%      1) the Key (mfad) isn't the same
%%      2) tail recursion
%%      When the previous depth is bigger than the current depth, which means
%%      that we just jump out of a sub function, we should not be adding
%%      new mfa_info either. <- this is the wrong assumption..... and the
%%      following snippet explains why:
%%      fib(0) ->
%%         0;
%%      fib(1) ->
%%         1;
%%      fib(N) ->
%%         fib(N-1) + fib(N-2).
%%      assuming that we want to calculate fib(3), when fib(2) is returned
%%      depth goes from 4 to 3 but the int goes directly to the fib(1) clause,
%%      whose mfa_info we should definitely add.
add_new_mfa_info_p(MFAInfo, NewKey, _Depth, NewLine) ->
  case is_key_of_hd_elem(NewKey, MFAInfo) of
    false ->
      true;
    true  ->
      MFAInfoElem = hd(MFAInfo),
      edts_rte_erlang:is_tail_recursion( MFAInfoElem#mfa_info.clauses_lines
                                       , MFAInfoElem#mfa_info.line
                                       , NewLine)
  end.

is_key_of_hd_elem(Key, MFAInfo) ->
  case get_hd(MFAInfo) of
    {ok, MFAInfo0} -> MFAInfo0#mfa_info.mfad =:= Key;
    false          -> false
  end.

get_hd([])     ->
  false;
get_hd([H|_T]) ->
  {ok, H}.

%%%_* Unit tests ===============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
