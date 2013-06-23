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

%% This server talks to the "rte int listener" to retrieve the binding info.
%% It keeps track of all the neccessary information for displaying the
%% function bodies with temp variables replaced. It also keeps track of the
%% record definition using an ets table, much like what the shell does.

%%%_* Module declaration =======================================================
-module(edts_rte_server).

-behaviour(gen_server).

%%%_* Exports =================================================================

%% server API
-export([start/0, stop/0, start_link/0]).

-export([started_p/0]).

%% APIs for the int listener
-export([ break_at/1
        , finished_attach/1
        , read_and_add_records/1
        , rte_run/3
        , send_exit/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Defines ==================================================================
-define(SERVER, ?MODULE).

-record(mfa_info, { key           :: mfa_info_key()
                  , fun_form      :: term()  %% FIXME type
                  , clauses_lines :: term()  %% FIXME type
                  , line          :: line()
                  , bindings      :: bindings()
                  }).

-record(rte_state, { proc          = unattached :: unattached
                                                 | pid()
                   , record_table  = undefined  :: atom()
                   , depth         = 0          :: depth()
                   , mfa_info_list = []         :: [mfa_info()]
                   , replaced_funs = []         :: list()  %% FIXME type
                   , result        = undefined  :: term()
                   , module_cache  = []         :: list()  %% FIXME type
                   }).

%%%_* Types ====================================================================
-type bindings()     :: [{atom(), any()}].
-type depth()        :: non_neg_integer().
-type line()         :: non_neg_integer().
-type mfa_info()     :: #mfa_info{}.
-type mfa_info_key() :: {module(), function(), arity(), depth()}.
-type state()        :: #rte_state{}.

-export_type([ {bindings, 0}
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
%% Run function through the RTE mechanism.
-spec rte_run(Module::module(), Fun::function(), Args::list()) -> {ok,finished}.
%%------------------------------------------------------------------------------
rte_run(Module, Fun, Args) ->
  gen_server:call(?SERVER, {rte_run, Module, Fun, Args}).

%%------------------------------------------------------------------------------
%% @doc Used by int listener to tell edts_rte_server that it has attached
%%      to the process that executes the rte function.
-spec finished_attach(pid()) -> ok.
finished_attach(Pid) ->
  gen_server:cast(?SERVER, {finished_attach, Pid}).

%%------------------------------------------------------------------------------
%% @doc Used by int listener to tell edts_rte_server that it has hit a break
%%      point with the bindings, module, line number and call stack depth
%%      information.
-spec break_at({bindings(), module(), line(), depth()}) -> ok.
break_at(Msg) ->
  gen_server:cast(?SERVER, {break_at, Msg}).

%%------------------------------------------------------------------------------
%% @doc Used by int listener to tell edts_rte_server that it has finished
%%      executing the function.
-spec send_exit() -> ok.
send_exit() ->
  gen_server:cast(?SERVER, exit).

%% FIXME: need to come up with a way to add all existing records from
%%        a project and remove records when recompile a particular module
read_and_add_records(Module) ->
  edts_rte_erlang:read_and_add_records(Module, record_table_name()).

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
  {ok, #rte_state{record_table = RcdTbl}}.

handle_call({rte_run, Module, Fun, Args0}, _From, State) ->
  RcdTbl   = State#rte_state.record_table,
  %% try to read the record from the current module.. right now this is the
  %% only record support
  AddedRds = edts_rte_erlang:read_and_add_records(Module, RcdTbl),
  edts_rte:debug("AddedRds:~p~n", [AddedRds]),
  Args     = binary_to_list(Args0),
  ArgsTerm = edts_rte_erlang:convert_list_to_term(Args, RcdTbl),
  edts_rte:debug("ArgsTerm:~p~n", [ArgsTerm]),
  [Module] = edts_rte_int_listener:interpret_modules([Module]),
  Arity    = length(ArgsTerm),
  edts_rte:debug("Arity:~p~n", [Arity]),
  {ok, set, {Module, Fun, Arity}} =
    edts_rte_int_listener:set_breakpoint(Module, Fun, Arity),
  edts_rte:debug("rte_run: after setbreakpoint~n"),
  RTEFun   = fun() ->
               Result = try
                          erlang:apply(Module, Fun, ArgsTerm)
                        catch
                          T:E -> io_lib:format("~p:~p", [T, E])
                        end,
               edts_rte:debug("RTE Result:~p~n", [Result]),
               send_rte_result(make_result({Module, Fun, Arity, Result}))
             end,
  Pid      = erlang:spawn(RTEFun),
  edts_rte:debug("called function pid:~p~n", [Pid]),
  {reply, {ok, finished}, State#rte_state{ depth         = 0
                                         , mfa_info_list = []
                                         , module_cache  = []
                                         , proc          = Pid
                                         , replaced_funs = []
                                         , result        = undefined
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
  %% edts_rte:debug("rte_server handle_info ...., Msg:~p~n", [Msg]),
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
  Pid = State#rte_state.proc,
  edts_rte_int_listener:step(),
  edts_rte:debug("finish attach.....~n"),
  {noreply, State};
handle_cast({break_at, {Bindings, Module, Line, Depth}},State0) ->
  {MFA, State} = get_mfa(State0, Module, Line),
  edts_rte:debug("1) send_binding.. before step. old depth:~p , new_depth:~p~n"
           , [State#rte_state.depth, Depth]),
  edts_rte:debug("2) send_binding.. Line:~p, Bindings:~p~n",[Line, Bindings]),

  edts_rte:debug("4) new mfa:~p~n", [MFA]),

  %% get mfa and add one level if it is not main function
  %% output sub function body when the process leaves it.
  %% Only step into one more depth right now.
  {ReplacedFuns, MFAInfoL} =
    case State#rte_state.depth > Depth of
      true  ->
        edts_rte:debug( "send replaced fun..~nbinding:~p~nDepth:~p~n"
                      , [Bindings, Depth]),

        %% Pop function up till function's depth <= Depth
        {OutputMFAInfo, RestMFAInfo} =
          lists:splitwith(fun(MFAInfoS) ->
                              {_M, _F, _A, D} = MFAInfoS#mfa_info.key,
                              D > Depth
                          end, State#rte_state.mfa_info_list),
        edts_rte:debug("4) OutputMFAInfo:~p~n", [OutputMFAInfo]),
        edts_rte:debug("5) RestMFAInfo:~p~n", [RestMFAInfo]),
        %% output function bodies
        OutputFuns = generate_replaced_funs(OutputMFAInfo) ++
                     State#rte_state.replaced_funs,
        {OutputFuns, RestMFAInfo};
      false ->
        {State#rte_state.replaced_funs, State#rte_state.mfa_info_list}
    end,

  MFAInfo = update_mfa_info_list(MFA, Depth, Line, Bindings, MFAInfoL),
  edts_rte_int_listener:step(),

  %% save current bindings for further use
  {noreply, State#rte_state{ depth = Depth
                           , mfa_info_list = MFAInfo
                           , replaced_funs = ReplacedFuns}};
handle_cast(exit, #rte_state{result = RteResult} = State) ->
  edts_rte:debug("rte server got exit~n"),
  AllReplacedFuns = generate_replaced_funs(State#rte_state.mfa_info_list) ++
                    State#rte_state.replaced_funs,
  send_result_to_clients(RteResult, concat_replaced_funs(AllReplacedFuns)),
  {noreply, State};
handle_cast({rte_result, Result}, State) ->
  edts_rte:debug("rte server got RTE Result:~p~n", [Result]),
  {noreply, State#rte_state{result = Result}};
handle_cast(_Msg, State) ->
  {noreply, State}.

generate_replaced_funs(MFAInfoList) ->
  lists:foldl(
    fun(MFAInfo, Funs) ->
           {M, F, A, D} = MFAInfo#mfa_info.key,
           MFA = {M, F, A},
           ReplacedFun = replace_var_in_fun_body(MFA, D, MFAInfo),
           [{M, F, A, ReplacedFun} | Funs]
          end, [], MFAInfoList).

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
%%------------------------------------------------------------------------------
%% @doc Send the rte result to the rte server.
-spec send_rte_result(term()) -> ok.
send_rte_result(Result) ->
  gen_server:cast(?SERVER, {rte_result, Result}).

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
  case orddict:find(Module, State#rte_state.module_cache) of
    error ->
      ModFunInfo  = edts_rte_erlang:get_module_sorted_fun_info(Module),
      NewModCache = orddict:store( Module, ModFunInfo
                                 , State#rte_state.module_cache),
      [_L, F, A]  = find_function(Line, ModFunInfo),
      {{Module, F, A}, State#rte_state{module_cache = NewModCache}};
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
  #mfa_info{ key          = Key
           , bindings      = Bindings
           , fun_form      = FunAbsForm
           , clauses_lines = AllClausesLn} = MFAInfoS,
  %% assert
  Key = erlang:append_element(MFA, Depth),
  edts_rte_erlang:var_to_val_in_fun(FunAbsForm, AllClausesLn, Bindings).

concat_replaced_funs(ReplacedFuns) ->
  lists:foldl(
    fun({M, F, A, RplFun}, RplFuns) ->
        lists:flatten(
          io_lib:format( "~s~n~s~n~s~n"
                       , [make_comments(M, F, A), RplFun, RplFuns]))
    end, [], ReplacedFuns).

make_result({M, F, A, RteResult}) ->
  lists:flatten(io_lib:format("%% ========== Generated by RTE ==========~n"
                              "%% ~p:~p/~p ---> ~p~n~n", [M, F, A, RteResult])).

%% @doc Make the comments to display on the client
make_comments(M, F, A) ->
  lists:flatten(io_lib:format("%% ========== Generated by RTE ==========~n"
                              "%% ========== MFA: {~p, ~p, ~p}:", [M, F, A])).

%% @doc  Either create a new mfa_info at the top of the mfa_info list
%%       or update the first element of the mfa_info list based on the
%%       new information. @see `add_new_mfa_info_p'
%%
%% NOTE: if the clauses are unfortunately programmed in the same line
%%       then rte shall feel confused and refuse to display any value of
%%       the variables.
-spec update_mfa_info_list( {module(), function(), arity()}, depth()
                          , line(), bindings(), [mfa_info()]) -> [mfa_info()].
update_mfa_info_list({M, F, A}, Depth, Line, Bindings, MFAInfoL) ->
  Key = {M, F, A, Depth},
  edts_rte:debug("6) params:~p~n", [[MFAInfoL, Key, Line]]),
  AddNewMFAInfoP = add_new_mfa_info_p(MFAInfoL, Key, Line),
  edts_rte:debug("7) app_p.......:~p~n", [AddNewMFAInfoP]),
  case AddNewMFAInfoP of
    true ->
      %% add new mfa_info
      {ok, FunAbsForm} = edts_code:get_function_body(M, F, A),
      AllClausesLn0    = edts_rte_erlang:extract_fun_clauses_line_num(
                           FunAbsForm),
      AllClausesLn     = edts_rte_erlang:traverse_clause_struct(
                           Line, AllClausesLn0),
      [ #mfa_info{ key           = Key
                 , line          = Line
                 , fun_form      = FunAbsForm
                 , clauses_lines = AllClausesLn
                 , bindings      = Bindings}
       | MFAInfoL];
    false ->
      #mfa_info{key= Key, clauses_lines = AllClausesLn} = Val = hd(MFAInfoL),
      TraversedLns = edts_rte_erlang:traverse_clause_struct(Line, AllClausesLn),
      [ Val#mfa_info{ clauses_lines = TraversedLns
                    , line          = Line
                    , bindings      = Bindings}
      | tl(MFAInfoL)]
  end.

%% @doc Return true when a new mfa_info needs to be added. This will happen
%%      when:
%%      1) The NewKey is not the same as that of the first element of the
%%         mfa_info_list.
%%      2) Tail recursion
-spec add_new_mfa_info_p([mfa_info()], mfa_info_key(), line()) -> boolean().
add_new_mfa_info_p(MFAInfoList, NewKey, NewLine) ->
  case key_of_first_elem_p(NewKey, MFAInfoList) of
    false ->
      true;
    true  ->
      MFAInfo = hd(MFAInfoList),
      edts_rte_erlang:is_tail_recursion( MFAInfo#mfa_info.clauses_lines
                                       , MFAInfo#mfa_info.line
                                       , NewLine)
  end.

%% @doc check if the key is the key of the first element in the MFAInfoList
%%      list.
-spec key_of_first_elem_p(mfa_info_key(), [mfa_info()]) -> boolean().
key_of_first_elem_p(Key, MFAInfoList) ->
  case try_get_hd(MFAInfoList) of
    {ok, MFAInfo} -> MFAInfo#mfa_info.key =:= Key;
    false         -> false
  end.

%% @doc Get the hd a list: `HEAD'. If the list is empty, return false.
%%      Otherwise return {ok, `Head'}
-spec try_get_hd(list()) -> false | {ok, any()}.
try_get_hd([])     ->
  false;
try_get_hd([H|_T]) ->
  {ok, H}.

%% @doc The name of the ETS table to store the tuple representation of
%%      the records
-spec record_table_name() -> atom().
record_table_name() ->
  edts_rte_record_table.

%% @doc Send the function body back to Clients.
send_result_to_clients(RteResult, FunBody) ->
  edts_rte:debug("final rte result:~p~n", [RteResult]),
  edts_rte:debug("final function body is:~p~n", [FunBody]),
  lists:foreach( fun(Fun) -> Fun(RteResult ++ FunBody) end
               , [ fun send_result_to_rte_web/1
                 , fun send_result_to_emacs/1
                 ]).

%% @doc Send the function body back to Emacs.
send_result_to_emacs(FunBody) ->
  BufferName = io_lib:format("*~s*", [make_id()]),
  EclientCmd = make_emacsclient_cmd(BufferName, FunBody),
  edts_rte:debug("FunBody:~p~n", [FunBody]),
  edts_rte:debug("emacsclient CMD:~p~n", [EclientCmd]),
  os:cmd(EclientCmd).

%% @doc Construct the emacsclient command to send the function
%%      body back to Emacs.
make_emacsclient_cmd(Id, FunBody) ->
  lists:flatten(io_lib:format(
                  "emacsclient -e '(edts-display-erl-fun-in-emacs"
                  " \"~s\" \"~s\" )'", [FunBody, Id])).

%% @doc Send the function body to rte web client.
send_result_to_rte_web(FunBody) ->
  httpc:request(post, { url(), [], content_type()
                      , mk_editor(make_id(), FunBody)}, [], []).

%% @doc Make the client id.
-spec make_id() -> string().
make_id() ->
  lists:flatten(io_lib:format("rte_result_~s", [node_str()])).

%% @doc Return the string representation of the node, replacing
%%      @ with __at__
-spec node_str() -> string().
node_str() ->
  re:replace(atom_to_list(node()), "@", "__at__", [{return, list}]).

url() ->
  "http://localhost:4587/rte/editors/".

content_type() ->
  "application/json".

%% @doc make an editor in json format which is comprehensible for
%%      the rte web client.
-spec mk_editor(string(), string()) -> string().
mk_editor(Id, FunBody) ->
 lists:flatten( io_lib:format(
                  "{\"x\":74,\"y\":92,\"z\":1,\"id\":\"~s\",\"code\":\"~s\"}"
              , [Id, FunBody])).

%%%_* Unit tests ===============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
