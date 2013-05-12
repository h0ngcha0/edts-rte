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
-define(RCDTBL, edts_rte_record_table).

-record(dbg_state, { proc         = unattached :: unattached | pid()
                   , bindings     = []         :: binding()
                   , mfa          = {}         :: {} | tuple()
                   , record_table = undefined
                   , depth        = 0          :: non_neg_integer()
                   , line         = undefined  :: non_neg_integer() | undefined
                   , mfa_form_map = undefined  :: dict()
                   }).

%%%_* Types ====================================================================
-type state()   :: #dbg_state{}.
-type binding() :: [{atom(), any()}].

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
  edts_rte_erlang:read_and_add_records(Module, ?RCDTBL).

record_table_name() ->
  ?RCDTBL.

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
  %% set the table to public to make debugging easier
  RcdTbl = ets:new(?RCDTBL, [public, named_table]),
  io:format("RcdTbl:~p", [RcdTbl]),
  {ok, #dbg_state{record_table = RcdTbl}}.

handle_call({rte_run, Module, Fun, Args0}, _From, State) ->
  RcdTbl   = State#dbg_state.record_table,
  %% try to read the record from this module.. right now this is the
  %% only record support
  AddedRds = edts_rte_erlang:read_and_add_records(Module, RcdTbl),
  io:format("AddedRds:~p~n", [AddedRds]),
  Args     = binary_to_list(Args0),
  ArgsTerm = edts_rte_erlang:convert_list_to_term(Args, RcdTbl),
  io:format("ArgsTerm:~p~n", [ArgsTerm]),
  [Module] = edts_rte_int_listener:interpret_modules([Module]),
  Arity    = length(ArgsTerm),
  io:format("Arity:~p~n", [Arity]),
  io:format("get function body after interpret~n"),
  {ok, set, {Module, Fun, Arity}} =
    edts_rte_int_listener:set_breakpoint(Module, Fun, Arity),
  io:format("rte_run: after setbreakpoint~n"),
  Pid      = erlang:spawn(Module, Fun, ArgsTerm),
  io:format("called function pid:~p~n", [Pid]),
  {reply, {ok, finished}, State#dbg_state{ proc         = Pid
                                         , bindings     = []
                                         , mfa          = {Module, Fun, Arity}
                                         , line         = undefined
                                         , depth        = 0
                                         , mfa_form_map = dict:new()
                                         }}.

%%------------------------------------------------------------------------------
%% @private
%% @doc Handling all non call/cast messages
%% @end
%%
-spec handle_info(term(), state()) -> {noreply, state()} |
                                      {noreply, state(), Timeout::timeout()} |
                                      {stop, Reason::atom(), state()}.
handle_info(_Msg, _State) ->
  %%io:format("in handle_info ...., break_at, Msg:~p~n", [Msg]),
  {noreply, _State}.

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

handle_cast({send_binding, {break_at, Bindings, Module, Line, Depth}}, State) ->
  io:format( "send_binding......before step. old depth:~p , new_depth:~p~n"
           , [State#dbg_state.depth, Depth]),
  io:format("send_binding......Line:~p, Bindings:~p~n",[Line, Bindings]),

  %% get mfa and add one level if it is not main function
  %% output sub function body when the process leaves it.
  %% Only step into one more depth right now.
  MFAFormMap =
    case State#dbg_state.depth > Depth of
      true  ->
        io:format( "in send_binding...:~n mfa:~p~nbinding:~p~nDepth:~p~n"
                 , [State#dbg_state.mfa, State#dbg_state.bindings, Depth]),
        {M, F, A} = State#dbg_state.mfa,
        %% Sub function call is finished, output subfunction body
        ReplacedFun = replace_fun_body( State#dbg_state.bindings
                                      , State#dbg_state.mfa
                                      , State#dbg_state.mfa_form_map),
        send_fun(M, F, A, ReplacedFun),
        cleanup_exec_clauses_linum( State#dbg_state.mfa
                                  , State#dbg_state.mfa_form_map);
      false ->
        State#dbg_state.mfa_form_map
    end,

  MFA = new_mfa(State, Module, Line, Depth),

  NewMFAFormMap = update_mfa_form_map(MFAFormMap, MFA, Line),

  io:format("old mfa:~p~n", [State#dbg_state.mfa]),
  io:format("new mfa:~p~n", [MFA]),
  edts_rte_int_listener:step(),

  %% save current bindings for further use
  {noreply, State#dbg_state{ bindings = Bindings, mfa = MFA
                           , depth = Depth, line = Line
                           , mfa_form_map = NewMFAFormMap }};

handle_cast(exit, #dbg_state{bindings = Bindings} = State) ->
  io:format( "in exit...:~n mfa:~p~nbinding:~p~n"
           , [State#dbg_state.mfa, Bindings]),
  {M, F, Arity} = State#dbg_state.mfa,
  ReplacedFun = replace_fun_body( Bindings, State#dbg_state.mfa
                                , State#dbg_state.mfa_form_map),
  send_fun(M, F, Arity, ReplacedFun),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

replace_fun_body(Bindings, MFA, MFAFormMap) ->
  io:format("dict 1~n"),
  {FunAbsForm, _AllClausesLn, ExecClausesLn} = dict:fetch(MFA, MFAFormMap),
  io:format("ExecClausesLn:~p~n", [ExecClausesLn]),
  edts_rte_erlang:var_to_val_in_fun(FunAbsForm, ExecClausesLn, Bindings).

cleanup_exec_clauses_linum(MFA, MFAFormMap) ->
  io:format("dict 2~n"),
  {FunAbsForm, AllClausesLn, ExecClausesLn} = dict:fetch(MFA, MFAFormMap),
  io:format("dict 7~n"),
  dict:store(MFA, {FunAbsForm, AllClausesLn, []}, MFAFormMap).

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
new_mfa(State, Module, Line, Depth) ->
  case State#dbg_state.depth =/= Depth of
    true  ->
      edts_rte_erlang:get_mfa_from_line(Module, Line);
    false ->
      State#dbg_state.mfa
  end.

send_fun(M, F, Arity, FunBody) ->
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

update_mfa_form_map(MFAFormMap, {M, F, A} = MFA, Line) ->
  io:format("dict 6~n"),
  case dict:is_key({M, F, A}, MFAFormMap) of
    true  ->
      io:format("dict 5~n"),
      {FunAbsForm, AllClausesLn, ExecClausesLn} =
        dict:fetch({M, F, A}, MFAFormMap),
      case lists:member(Line, AllClausesLn) of
        true  ->   io:format("dict 4~n"),
                   dict:store( {M, F, A}
                           , {FunAbsForm, AllClausesLn, [Line|ExecClausesLn]}
                           , MFAFormMap);
        false -> MFAFormMap
      end;
    false ->
      {ok, FunAbsForm} = edts_code:get_function_body(M, F, A),
      AllClausesLn     = edts_rte_erlang:extract_fun_clauses_line_num(
                           FunAbsForm),
      io:format("dict 3~n"),
      dict:store({M, F, A}, {FunAbsForm, AllClausesLn, []}, MFAFormMap)
  end.

%%%_* Unit tests ===============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
