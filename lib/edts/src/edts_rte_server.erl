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
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Defines ==================================================================
-define(SERVER, ?MODULE).
-record(dbg_state, { proc     = unattached :: unattached | pid()
                   , bindings = []         :: binding()
                   , mfa      = {}         :: {} | tuple()
                   }).

%%%_* Types ====================================================================
-type state()   :: #dbg_state{}.
-type binding() :: [{atom(), any()}].

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
  {ok, #dbg_state{}}.

handle_call({rte_run, Module, Fun, Args}, _From, State) ->
  ok       = edts_rett_server:set_rte_flag(),
  [Module] = edts_rett_server:interpret_modules([Module]),
  Arity    = length(Args),
  io:format("get function body after interpret~n"),
  {ok, set, {Module, Fun, Arity}} =  edts_rett_server:set_breakpoint(Module, Fun, Arity),
  io:format("rte_run: after setbreakpoint~n"),
  Pid      = erlang:spawn(Module, Fun, Args),
  io:format("called function pid:~p~n", [Pid]),
  {reply, {ok, finished}, State#dbg_state{ proc = Pid
                                         , bindings = []
                                         , mfa = {Module, Fun, Arity}}}.

%%------------------------------------------------------------------------------
%% @private
%% @doc Handling all non call/cast messages
%% @end
%%
-spec handle_info(term(), state()) -> {noreply, state()} |
                                      {noreply, state(), Timeout::timeout()} |
                                      {stop, Reason::atom(), state()}.
handle_info(Msg, State) ->
  io:format("in handle_info ...., break_at, Msg:~p~n", [Msg]),
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
  edts_rett_server:step(),
  io:format("finish attach.....~n"),
  {noreply, State};

handle_cast({send_binding, {break_at, Bindings}}, State) ->
  io:format("send_binding......before step~n"),
  edts_rett_server:step(),
  io:format("send_binding......Bindings:~p~n",[Bindings]),
  {noreply, State#dbg_state{bindings = Bindings}};

handle_cast(exit, #dbg_state{bindings = Bindings} = State) ->
  %%io:format("in exit, Bindings:~p~n", [Bindings]),
  %% get function body
  {M, F, Arity}  = State#dbg_state.mfa,
  {ok, Body} = edts_code:get_function_body(M, F, Arity),
  io:format( "output FunBody, Bindings before replace:~n"++Body++"~n"),
  io:format( "Bindings:~n~p~n", [Bindings]),
  %% replace function body with bindings
  ReplacedFun = replace_var_with_val_in_fun(Body, Bindings),
  io:format( "output funbody after replacement:~n"++ReplacedFun++"~n"),
  send_fun(M, F, Arity, ReplacedFun),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

send_fun(M, F, Arity, FunBody) ->
  lists:foreach(fun(Fun) ->
                    Fun(M, F, Arity, FunBody)
                end, [ fun send_fun_to_edts/4
                     , fun send_fun_to_emacs/4]).

send_fun_to_emacs(M, F, Arity, FunBody) ->
  Id = lists:flatten(io_lib:format("~p__~p__~p", [M, F, Arity])),
  Cmd = make_emacsclient_cmd(Id, FunBody),
  io:format("cmd:~p~n", [Cmd]),
  os:cmd(Cmd).

make_emacsclient_cmd(Id, FunBody) ->
  "emacsclient "++"\"(edts-display-erl-fun-in-emacs "
    ++ "\"" ++ FunBody ++ "\" " ++ "\"" ++ Id ++ "\"" ++")\"".

send_fun_to_edts(M, F, Arity, FunBody) ->
  Id  = lists:flatten(io_lib:format("~p__~p__~p", [M, F, Arity])),
  httpc:request(post, {url(), [], content_type(), mk_editor(Id, FunBody)}, [], []).

url() ->
  "http://localhost:4587/rte/editors/".

content_type() ->
  "application/json".

mk_editor(Id, FunBody) ->
  "{\"x\":74,\"y\":92,\"z\":1,\"id\":\""++Id++"\",\"code\":\""++FunBody++"\"}".

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

%% @doc replace the temporary variables with the actual value in a function
-spec replace_var_with_val_in_fun( FunBody :: string()
                                 , Bindings :: binding()) -> string().
replace_var_with_val_in_fun(FunBody, Bindings) ->
  %% Parse function body to AbsForm
  {ok, FunBodyToken, _} = erl_scan:string(FunBody),
  {ok, AbsForm}         = erl_parse:parse_form(FunBodyToken),
  %% Replace variable names with variables' value and
  %% combine the Token to function string again
  NewFunBody            = do_replace_var_with_val_in_fun( AbsForm
                                                        , Bindings),
  io:format("New Body before flatten: ~p~n", [NewFunBody]),
  NewForm               = erl_pp:form(NewFunBody),
  lists:flatten(NewForm).

%% @doc replace variable names with values for a function
do_replace_var_with_val_in_fun( {function, L, FuncName, Arity, Clauses0}
                              , Bindings) ->
  Clauses = replace_var_with_val_in_clauses(Clauses0, Bindings),
  io:format("Replaced Clauses are:~p~n", [Clauses0]),
  {function, L, FuncName, Arity, Clauses}.

%% @doc replace variable names with values in each of the function clauses
replace_var_with_val_in_clauses([], _Bindings)                         ->
  [];
replace_var_with_val_in_clauses([ {clause,L,ArgList0,WhenList0,Lines0}|T]
                                , Bs)                                  ->
  %% replace variables' name with values in argument list
  ArgList  = replace_var_with_val_args(ArgList0, Bs),
  %% replace variables' name with values in "when" list
  WhenList = replace_var_with_val_args(WhenList0, Bs),
  %% replace variables' name with values for each of the expressions
  Lines    = replace_var_with_val_in_expr(Lines0, Bs),
  [ {clause,L,ArgList,WhenList,Lines}
  | replace_var_with_val_in_clauses(T, Bs)].

replace_var_with_val_args([], _Bindings)->[];
replace_var_with_val_args([VarExpr0|T], Bindings) ->
  VarExpr = replace_var_with_val(VarExpr0, Bindings),
  [VarExpr | replace_var_with_val_args(T, Bindings)].

replace_var_with_val_in_expr([], _Bindings)                               ->
  [];
replace_var_with_val_in_expr(Atom, _Bindings) when is_atom(Atom)          ->
  Atom;
replace_var_with_val_in_expr({nil, L}, _Bindings)                         ->
  {nil, L};
replace_var_with_val_in_expr({atom, _L, _A} = VarExpr, _Bindings)         ->
  VarExpr;
replace_var_with_val_in_expr({cons, L, Expr0, Rest0}, Bindings)           ->
  Expr = replace_var_with_val_in_expr(Expr0, Bindings),
  Rest = replace_var_with_val_in_expr(Rest0, Bindings),
  {cons, L, Expr, Rest};
replace_var_with_val_in_expr({tuple, L, Exprs0}, Bindings)                ->
  Exprs = lists:map(fun(Expr) ->
                        replace_var_with_val_in_expr(Expr, Bindings)
                    end, Exprs0),
  {tuple, L, Exprs};
replace_var_with_val_in_expr({float, _, _} = VarExpr, _Bindings)          ->
  VarExpr;
replace_var_with_val_in_expr({integer, _, _} = VarExpr, _Bindings)        ->
  VarExpr;
replace_var_with_val_in_expr({match,L,LExpr0,RExpr0}, Bindings)           ->
  LExpr = replace_var_with_val_in_expr(LExpr0, Bindings),
  RExpr = replace_var_with_val_in_expr(RExpr0, Bindings),
  {match,L,LExpr,RExpr};
replace_var_with_val_in_expr({var, _, _} = VarExpr, Bindings)             ->
  replace_var_with_val(VarExpr, Bindings);
replace_var_with_val_in_expr({op, _, _, _, _} = OpsExpr, Bindings)        ->
  replace_var_with_val_ops(OpsExpr, Bindings);
replace_var_with_val_in_expr({call, L, {atom, L, F0}, ArgList0}, Bindings) ->
  F = replace_var_with_val_in_expr(F0, Bindings),
  {call, L, {atom, L, F}, replace_var_with_val_args(ArgList0, Bindings)};
replace_var_with_val_in_expr({call, L, {remote, L, M0, F0}, Args0}, Bindings) ->
  M = replace_var_with_val_in_expr(M0, Bindings),
  F = replace_var_with_val_in_expr(F0, Bindings),
  {call, L, {remote, L, M, F}, replace_var_with_val_args(Args0, Bindings)};
replace_var_with_val_in_expr([Statement0|T], Bindings)                    ->
  Statement = replace_var_with_val_in_expr(Statement0, Bindings),
  [Statement | replace_var_with_val_in_expr(T, Bindings)].

replace_var_with_val_ops({op, L, Ops, LExpr0, RExpr0}, Bindings)  ->
  LExpr = replace_var_with_val_in_expr(LExpr0, Bindings),
  RExpr = replace_var_with_val_in_expr(RExpr0, Bindings),
  {op, L, Ops, LExpr, RExpr}.

replace_var_with_val({var, L, VariableName}, Bindings) ->
  Value = proplists:get_value(VariableName, Bindings),
  io:format("VarName:~p   L:~p    Val:~p~n", [VariableName, L, Value]),
  Val = do_replace(Value, L),
  io:format("replaced Var:~p~n", [Val]),
  Val;
replace_var_with_val(Other, _Bindings)                 ->
  Other.

do_replace(Value, L) ->
  ValStr          = lists:flatten(io_lib:format("~p.", [Value])),
  {ok, Token, _}  = erl_scan:string(ValStr),
  {ok, [ValForm]} = erl_parse:parse_exprs(Token),
  replace_line_num(ValForm, L).

replace_line_num({A, _L0, C, D}, L)               ->
  {A, L, replace_line_num(C, L), replace_line_num(D, L)};
replace_line_num({A, _L0, C},    L)               ->
  {A, L, replace_line_num(C, L)};
replace_line_num({A, _L0},       L)               ->
  {A, L};
replace_line_num(Others,  L) when is_list(Others) ->
  lists:map(fun(Other) ->
                replace_line_num(Other, L)
            end, Others);
replace_line_num(Other,  _L)                      ->
  Other.

%%%_* Unit tests ===============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
