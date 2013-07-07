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

-record(mfa_info, { key            :: mfa_info_key()
                  , fun_form       :: term()  %% FIXME type
                  , clause_structs :: term()  %% FIXME type
                  , line           :: line()
                  , bindings       :: bindings()
                  , is_current     :: boolean()
                  , children       :: [mfa_info()]
                  }).

-record(rte_state, { proc                  = unattached :: unattached
                                                         | pid()
                   , record_table          = undefined  :: atom()
                   , mfa_info_tree         = []         :: list()  %% FIXME type
                   , result                = undefined  :: term()
                   , module_cache          = []         :: list()  %% FIXME type
                   , exit_p                = false      :: boolean()
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
  %% try to read the record from the current module.. right now this is the
  %% only record support
  RcdTbl   = State#rte_state.record_table,
  AddedRds = edts_rte_erlang:read_and_add_records(Module, RcdTbl),
  edts_rte:debug("added record definitions:~p~n", [AddedRds]),

  Args     = binary_to_list(Args0),
  ArgsTerm = edts_rte_erlang:convert_list_to_term(Args, RcdTbl),
  edts_rte:debug("arguments:~p~n", [ArgsTerm]),

  %% set breakpoints
  [Module] = edts_rte_int_listener:interpret_modules([Module]),
  Arity    = length(ArgsTerm),
  {ok, set, {Module, Fun, Arity}} =
    edts_rte_int_listener:set_breakpoint(Module, Fun, Arity),

  %% run mfa
  RTEFun   = make_rte_run(Module, Fun, ArgsTerm),
  Pid      = erlang:spawn(RTEFun),
  edts_rte:debug("called function pid:~p~n", [Pid]),

  %% root element of the mfa_info_tree
  MFAInfo  = #mfa_info{ key        = {undefined, undefined, undefined, 0}
                      , is_current = true
                      , children   = []
                      },

  {reply, {ok, finished}, State#rte_state{ module_cache          = []
                                         , proc                  = Pid
                                         , mfa_info_tree         = [MFAInfo]
                                         , result                = undefined
                                         , exit_p                = false
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
handle_cast({break_at, {Bindings, Module, Line, Depth}}, State0) ->
  {MFA, State} = get_mfa(State0, Module, Line),
  edts_rte:debug("1) send_binding.. before step. depth:~p~n", [Depth]),
  edts_rte:debug("2) send_binding.. Line:~p, Bindings:~p~n",[Line, Bindings]),

  edts_rte:debug("3) new mfa:~p~n", [MFA]),

  NewMFAInfoTree = update_mfa_info_tree( MFA, Depth, Line, Bindings
                                       , State#rte_state.mfa_info_tree),

  %% continue to step
  edts_rte_int_listener:step(),

  {noreply, State#rte_state{mfa_info_tree = NewMFAInfoTree}};
handle_cast(exit, #rte_state{result = RteResult} = State0) ->
  edts_rte:debug("rte server got exit~n"),
  State = on_exit(RteResult, State0),
  {noreply, State#rte_state{exit_p=true}};
handle_cast({rte_result, Result}, #rte_state{exit_p = ExitP} = State) ->
  edts_rte:debug("rte server got RTE Result:~p~n", [Result]),
  ExitP andalso on_exit(Result, State),
  {noreply, State#rte_state{result=Result}};
handle_cast(_Msg, State) ->
  {noreply, State}.

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

%% @doc Generate the replaced function based on the mfa_info
make_replaced_fun(MFAInfoS) ->
  #mfa_info{ bindings       = Bindings
           , fun_form       = FunAbsForm
           , clause_structs = AllClausesLn} = MFAInfoS,
  edts_rte_erlang:var_to_val_in_fun(FunAbsForm, AllClausesLn, Bindings).

%% @doc Called when an RTE run is about to finish. Generate the replaced
%%      functions and send them to the clients.
-spec on_exit(undefined | string(), state()) -> state().
on_exit(undefined, State) ->
  State;
on_exit(Result, State) ->
  AllReplacedFuns = print_mfa_info_tree(State#rte_state.mfa_info_tree),
  ok = send_result_to_clients(Result, concat_replaced_funs(AllReplacedFuns)),
  edts_rte:debug("======= mfa_info_tree: ~p~n",[State#rte_state.mfa_info_tree]),
  State.

%% @doc Concat a list of replaced function strings together.
-spec concat_replaced_funs([{Key, string()}]) -> string()
         when Key :: {module(), function(), arity()}.
concat_replaced_funs(ReplacedFuns) ->
  lists:foldl(
    fun({{M, F, A, D}, RplFun}, RplFuns) ->
        lists:flatten(
          io_lib:format( "~s~n~s~n~s~n"
                       , [make_comments(M, F, A, D), RplFun, RplFuns]))
    end, [], ReplacedFuns).

make_result({M, F, A, RteResult}) ->
  lists:flatten(io_lib:format("%% ========== Generated by RTE ==========~n"
                              "%% ~p:~p/~p ---> ~p~n~n", [M, F, A, RteResult])).

%% @doc Make the comments to display on the client
make_comments(M, F, A, D) ->
  lists:flatten(io_lib:format("%% MFA   : {~p, ~p, ~p}:~n"
                              "%% Level : ~p", [M, F, A, D])).

%% @doc Update the mfa_info_tree.
%%      The current mfa_info element should only be along the
%%      rightmost line of nodes in the mfa_info_tree.
update_mfa_info_tree({Mod, Fun, Arity}, Depth, Line, Bindings, []) ->
  [new_mfa_info(Mod, Fun, Arity, Depth, Line, Bindings)];
update_mfa_info_tree( {Mod, Fun, Arity}, Depth, Line, Bindings
                    , [#mfa_info{is_current = true} = MFAInfo]) ->
  case MFAInfo#mfa_info.key =:= {Mod, Fun, Arity, Depth} of
    true  ->
      %% assert that it can not be a tail call here. because
      %% the tail call scenario should be handled by the
      %% ancester of this element already.
      false  = edts_rte_erlang:is_tail_call(
                 MFAInfo#mfa_info.clause_structs,
                 MFAInfo#mfa_info.line, Line),

      %% the interpreter steps forward within the same function, so
      %% just need to update the current mfa_info.
      [update_mfa_info(MFAInfo, Line, Bindings)];
    false ->
      {_Mod, _Fun, _Arity, DepthOfElem} = MFAInfo#mfa_info.key,
      %% assert that the current depth is bigger than the depth
      %% of this element. The cases where the current depths is
      %% smaller or equal than the depth of this element should
      %% be handled already by the ancester of this element.
      true = Depth > DepthOfElem,

      %% add a new child element at the end of the children list of
      %% the current mfa_info list.
      [add_child(Mod, Fun, Arity, Depth, Line, Bindings, MFAInfo)]
  end;
update_mfa_info_tree( {Mod, Fun, Arity}, Depth, Line, Bindings
                    , [#mfa_info{is_current = false} = MFAInfo]) ->
  Children  = MFAInfo#mfa_info.children,
  LastChild = lists:last(Children),
  case LastChild#mfa_info.is_current of
    true ->
      case MFAInfo#mfa_info.key =:= {Mod, Fun, Arity, Depth} of
        true ->
          %% if the interpreter just steps back to the father node
          %% just need to update this mfa_info element
          [update_self(MFAInfo, Line, Bindings)];
        false ->
          {_Mod0, _Fun0, _Arity0, DepthOfElem} = MFAInfo#mfa_info.key,
          %% assert
          true = Depth > DepthOfElem,

          Key = {Mod, Fun, Arity, Depth},
          case add_sibling_p(LastChild, Key, Line, Depth) of
            true ->
              %% Create a sibling if needed.
              [add_child(Mod, Fun, Arity, Depth, Line, Bindings, MFAInfo)];
            false ->
              %% Otherwise continue the same process with the children
              NewChildren = update_mfa_info_tree( {Mod, Fun, Arity}, Depth, Line
                                                , Bindings, Children),
              [MFAInfo#mfa_info{children = NewChildren}]
          end
      end;
    false ->
      case add_sibling_p(LastChild, {Mod, Fun, Arity, Depth}, Line, Depth) of
        true  ->
          [add_child(Mod, Fun, Arity, Depth, Line, Bindings, MFAInfo)];
        false ->
          case MFAInfo#mfa_info.key =:= {Mod, Fun, Arity, Depth} of
            true ->
              %% if the interpreter just steps back to the GRAND-father node
              %% just need to update this mfa_info element
              [update_self(MFAInfo, Line, Bindings)];
            false ->
              %% Otherwise continue the same process with the children
              NewChildren = update_mfa_info_tree( {Mod, Fun, Arity}, Depth, Line
                                                , Bindings, Children),
              [MFAInfo#mfa_info{children = NewChildren}]
          end
      end
  end;
update_mfa_info_tree({M, F, A}, Depth, Line, Bindings, [H|T]) ->
  %% only look at the rightmost line of mfa_info elements in the tree.
  [H|update_mfa_info_tree({M, F, A}, Depth, Line, Bindings, T)].

%% @doc Update current mfa_info element.
-spec update_self(mfa_info(), line(), bindings()) -> mfa_info().
update_self(MFAInfo0, Line, Bindings) ->
  Children = set_current_false(MFAInfo0#mfa_info.children),
  MFAInfo  = MFAInfo0#mfa_info{ is_current = true
                              , children   = Children},
  update_mfa_info(MFAInfo, Line, Bindings).

%% @doc add a new child.
-spec add_child( module(), function(), arity(), depth(), line()
                 , bindings(), mfa_info()) -> mfa_info().
add_child(Mod, Fun, Arity, Depth, Line, Bindings, MFAInfo) ->
  Children    = MFAInfo#mfa_info.children,
  NewSibling  = new_mfa_info( Mod, Fun, Arity, Depth, Line, Bindings),
  NewChildren = set_current_false(Children) ++ [NewSibling],
  MFAInfo#mfa_info{ is_current = false
                  , children = NewChildren}.  
  
%% @doc Check if a sibling should be added for a mfa_info element. This
%%      could happen in two scenarios:
%%      1) the mfa is not the same, but the depth is the same
%%      2) tail call
add_sibling_p(MFAInfo, NewKey, NewLine, NewDepth) ->
  case MFAInfo#mfa_info.key =:= NewKey of
    true  ->
      %% this will rule out the case where we are stepping within
      %% the same function clause.
      edts_rte_erlang:is_tail_call( MFAInfo#mfa_info.clause_structs
                                  , MFAInfo#mfa_info.line
                                  , NewLine);
    false ->
      {_M, _F, _A, Depth} = MFAInfo#mfa_info.key,
      NewDepth =:= Depth
  end.

%% @doc set the is_current property of the last sibling of this
%%      mfa_info list.
-spec set_current_false([mfa_info()]) -> [mfa_info()].
set_current_false([])       ->
  [];
set_current_false([MFAInfo|T]) ->
  Children = MFAInfo#mfa_info.children,
  [ MFAInfo#mfa_info{is_current = false, children = set_current_false(Children)}
  | set_current_false(T)].

%% @doc Create a new mfa_info element
-spec new_mfa_info(module(), function(), arity(), depth(), line(), bindings())
                  -> mfa_info().
new_mfa_info(Module, Function, Arity, Depth, Line, Bindings) ->
  {ok, FunAbsForm} = edts_code:get_function_body(Module, Function, Arity),
  AllClausesL0     = edts_rte_erlang:extract_fun_clauses_line_num(FunAbsForm),
  AllClausesL      = edts_rte_erlang:traverse_clause_struct(Line, AllClausesL0),
  #mfa_info{ key            = {Module, Function, Arity, Depth}
           , line           = Line
           , fun_form       = FunAbsForm
           , clause_structs = AllClausesL
           , is_current     = true
           , children       = []
           , bindings       = Bindings}.

%% @doc Update the mfa_info element
-spec update_mfa_info(mfa_info(), line(), bindings()) -> mfa_info().
update_mfa_info(MFAInfo, Line, Bindings) ->
  ClauseStructs = edts_rte_erlang:traverse_clause_struct(
                    Line, MFAInfo#mfa_info.clause_structs),
  MFAInfo#mfa_info{ clause_structs = ClauseStructs
                  , line           = Line
                  , bindings       = Bindings}.

print_mfa_info_tree([MFAInfo])   ->
  lists:reverse(do_print_mfa_info_tree(MFAInfo#mfa_info.children, [])).

do_print_mfa_info_tree([], Acc) ->
  Acc;
do_print_mfa_info_tree([MFAInfo|T], Acc) ->
  ReplacedFun = make_replaced_fun(MFAInfo),
  AccChildren = do_print_mfa_info_tree(MFAInfo#mfa_info.children, []),
  do_print_mfa_info_tree(T, Acc ++ [{MFAInfo#mfa_info.key, ReplacedFun} | AccChildren]).

%% @doc The name of the ETS table to store the tuple representation of
%%      the records
-spec record_table_name() -> atom().
record_table_name() ->
  edts_rte_record_table.

%% @doc Send the function body back to Clients.
send_result_to_clients(RteResult, FunBody) ->
  edts_rte:debug("final rte result:~p~n", [RteResult]),
  edts_rte:debug("final function body is:~p~n", [FunBody]),
  Result = string_escape_chars(RteResult ++ FunBody, escaped_chars()),
  lists:foreach( fun(Fun) -> Fun(Result) end
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

%% @doc Make the function to execute the MFA.
-spec make_rte_run(module(), function(), [term()]) -> fun(() -> ok).
make_rte_run(Module, Fun, ArgsTerm) ->
  fun() ->
    Result = try
               erlang:apply(Module, Fun, ArgsTerm)
             catch
               T:E -> lists:flatten(io_lib:format("~p:~p", [T, E]))
             end,
    edts_rte:debug("RTE Result:~p~n", [Result]),
    send_rte_result(make_result({Module, Fun, length(ArgsTerm), Result}))
  end.

%% @doc Escape the chars in a given list from a string.
-spec string_escape_chars(string(), [string()]) -> string().
string_escape_chars(Msg, Chars) ->
  Fun = fun(C, MsgAcc) ->
          re:replace(MsgAcc, "\\"++C, "\\\\"++C, [global, {return, list}])
        end,
  lists:foldl(Fun, Msg, Chars).

escaped_chars() ->
  ["\""].

%%%_* Unit tests ===============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
