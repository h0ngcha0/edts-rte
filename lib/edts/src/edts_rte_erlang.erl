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

%% This Module deals with stuff related to the erlang internal programing
%% constructs.
%% Record related stuff are shamelessly copied from shell.erl

%%%_* Module declaration =======================================================
-module(edts_rte_erlang).

%%%_* Exports =================================================================
-export([ convert_list_to_term/2
        , expand_records/2
        , read_and_add_records/2
        , var_to_val_in_fun/3
        , get_mfa_from_line/2
        ]).

%%%_* Includes =================================================================
-include_lib("kernel/include/file.hrl").

%%%_* API ======================================================================
convert_list_to_term(Arguments, _RT) ->
  io:format("args:~p~n", [Arguments]),
  %% N.B. this is very hackish. added a '.' because
  %%      erl_scan:string/1 requires full expression with dot
  {ok, Tokens,__Endline} = erl_scan:string(Arguments++"."),
  io:format("tokens:~p~n", [Tokens]),
  {ok, AbsForm0}         = erl_parse:parse_exprs(Tokens),
  AbsForm                = replace_var_with_val_in_expr(AbsForm0, []),
  io:format("absf:~p~n", [AbsForm0]),
  Val     = erl_eval:exprs( AbsForm
                          , erl_eval:new_bindings()),
  io:format("Valg:~p~n", [Val]),
  {value, Value,_Bs} = Val,
  io:format("val:~p~n", [Value]),
  Value.

expand_records(RT, E0) ->
  UsedRecords = used_record_defs(E0, RT),
  do_expand_records(UsedRecords, E0).

read_and_add_records(Module, RT) ->
  read_and_add_records(Module, '_', [], [], RT).

get_mfa_from_line(M, L0) ->
  FAs = int:functions(M),
  AllFuns = lists:foldl(
    fun([F, A], AllFuns) ->
        FunInfo = edts_code:get_function_info(M, F, A),
        {line, Line} = lists:keyfind(line, 1, FunInfo),
        [[Line, F, A] | AllFuns] end, [], FAs),
  SortedAllFuns = lists:reverse(lists:sort(AllFuns)),
  %% [Line, F, A]
  [_L, F, A] = find_function(L0, SortedAllFuns),
  {M, F, A}.

%%%_* Internal =================================================================

find_function(_L, [])                    ->
  [];
find_function(L, [[L0, _F, _A] = LFA | T]) ->
  case L >= L0 of
    true  -> LFA;
    false -> find_function(L, T)
  end.

read_and_add_records(Module, Selected, Options, Bs, RT) ->
  Info             = edts_code:get_module_info(Module, basic),
  {source, Source} = lists:keyfind(source, 1, Info),
  case read_records(Source, Selected, Options) of
    RAs when is_list(RAs) ->
      add_records(RAs, Bs, RT);
    Error ->
      Error
  end.

read_records(File, Selected, Options) ->
  case read_records(File, listify(Options)) of
    Error when is_tuple(Error) ->
      Error;
    RAs when Selected =:= '_' ->
      RAs;
    RAs ->
      Sel = listify(Selected),
      [RA || {attribute,_,_,{Name,_}}=RA <- RAs,
             lists:member(Name, Sel)]
  end.

add_records(RAs, Bs0, RT) ->
  Recs = [{Name,D} || {attribute,_,_,{Name,_}}=D <- RAs],
  Bs1 = record_bindings(Recs, Bs0),
  case check_command([], Bs1) of
    {error,{_Line,M,ErrDesc}} ->
      %% A source file that has not been compiled.
      ErrStr = io_lib:fwrite(<<"~s">>, [M:format_error(ErrDesc)]),
      exit(lists:flatten(ErrStr));
    ok ->
      true = ets:insert(RT, Recs),
      lists:usort([Name || {Name,_} <- Recs])
  end.

listify(L) when is_list(L) ->
  L;
listify(E) ->
  [E].

read_records(FileOrModule, Opts0) ->
  Opts = lists:delete(report_warnings, Opts0),
  case find_file(FileOrModule) of
    {files,[File]} ->
      read_file_records(File, Opts);
    {files,Files} ->
      lists:flatmap(fun(File) ->
                        case read_file_records(File, Opts) of
                          RAs when is_list(RAs) -> RAs;
                          _ -> []
                        end
                    end, Files);
    Error ->
      Error
  end.

%% Note that a sequence number is used here to make sure that if a
%% record is used by another record, then the first record is parsed
%% before the second record. (erl_eval:check_command() calls the
%% linter which needs the records in a proper order.)
record_bindings([], Bs) ->
  Bs;
record_bindings(Recs0, Bs0) ->
  {Recs1, _} = lists:mapfoldl(fun ({Name,Def}, I) -> {{Name,I,Def},I+1}
                              end, 0, Recs0),
  Recs2 = lists:keysort(2, lists:ukeysort(1, Recs1)),
  lists:foldl(fun ({Name,I,Def}, Bs) ->
                  erl_eval:add_binding({record,I,Name}, Def, Bs)
              end, Bs0, Recs2).

check_command(Es, Bs) ->
  erl_eval:check_command(Es, strip_bindings(Bs)).

find_file(Mod) when is_atom(Mod) ->
  case code:which(Mod) of
      File when is_list(File) ->
        {files,[File]};
      preloaded ->
        {_M,_Bin,File} = code:get_object_code(Mod),
        {files,[File]};
      _Else -> % non_existing, interpreted, cover_compiled
        {error,nofile}
    end;
find_file(File) ->
  case catch filelib:wildcard(File) of
    {'EXIT',_} ->
      {error,invalid_filename};
    Files ->
      {files,Files}
  end.

read_file_records(File, Opts) ->
  case filename:extension(File) of
    ".beam" ->
      case beam_lib:chunks(File, [abstract_code,"CInf"]) of
        {ok,{_Mod,[{abstract_code,{Version,Forms}},{"CInf",CB}]}} ->
          case record_attrs(Forms) of
            [] when Version =:= raw_abstract_v1 ->
              [];
            [] ->
              %% If the version is raw_X, then this test
              %% is unnecessary.
              try_source(File, CB);
            Records ->
              Records
          end;
        {ok,{_Mod,[{abstract_code,no_abstract_code},{"CInf",CB}]}} ->
          try_source(File, CB);
        Error ->
          %% Could be that the "Abst" chunk is missing (pre R6).
          Error
      end;
    _ ->
      parse_file(File, Opts)
  end.

-spec strip_bindings(erl_eval:binding_struct()) -> erl_eval:binding_struct().

strip_bindings(Bs) ->
  Bs -- [B || {{module,_},_}=B <- Bs].

record_attrs(Forms) ->
  [A || A = {attribute,_,record,_D} <- Forms].

%% This is how the debugger searches for source files. See int.erl.
try_source(Beam, CB) ->
  Os = case lists:keyfind(options, 1, binary_to_term(CB)) of
         false -> [];
         {_, Os0} -> Os0
       end,
  Src0 = filename:rootname(Beam) ++ ".erl",
  case is_file(Src0) of
    true -> parse_file(Src0, Os);
    false ->
      EbinDir = filename:dirname(Beam),
      Src = filename:join([filename:dirname(EbinDir), "src",
                           filename:basename(Src0)]),
      case is_file(Src) of
        true -> parse_file(Src, Os);
        false -> {error, nofile}
      end
  end.

parse_file(File, Opts) ->
  Cwd = ".",
  Dir = filename:dirname(File),
  IncludePath = [Cwd,Dir|inc_paths(Opts)],
  case epp:parse_file(File, IncludePath, pre_defs(Opts)) of
    {ok,Forms} ->
      record_attrs(Forms);
    Error ->
      Error
  end.

pre_defs([{d,M,V}|Opts]) ->
  [{M,V}|pre_defs(Opts)];
pre_defs([{d,M}|Opts]) ->
  [M|pre_defs(Opts)];
pre_defs([_|Opts]) ->
  pre_defs(Opts);
pre_defs([]) -> [].

inc_paths(Opts) ->
  [P || {i,P} <- Opts, is_list(P)].

is_file(Name) ->
  case filelib:is_file(Name) of
    true ->
      not filelib:is_dir(Name);
    false ->
      false
  end.

used_record_defs(E, RT) ->
    %% Be careful to return a list where used records come before
    %% records that use them. The linter wants them ordered that way.
    UR = case used_records(E, [], RT) of
             [] ->
                 [];
             L0 ->
                 L1 = lists:zip(L0, lists:seq(1, length(L0))),
                 L2 = lists:keysort(2, lists:ukeysort(1, L1)),
                 [R || {R, _} <- L2]
         end,
    record_defs(RT, UR).

used_records(E, U0, RT) ->
    case used_records(E) of
        {name,Name,E1} ->
            U = used_records(ets:lookup(RT, Name), [Name | U0], RT),
            used_records(E1, U, RT);
        {expr,[E1 | Es]} ->
            used_records(Es, used_records(E1, U0, RT), RT);
        _ ->
            U0
    end.

used_records({record_index,_,Name,F}) ->
    {name, Name, F};
used_records({record,_,Name,Is}) ->
    {name, Name, Is};
used_records({record_field,_,R,Name,F}) ->
    {name, Name, [R | F]};
used_records({record,_,R,Name,Ups}) ->
    {name, Name, [R | Ups]};
used_records({record_field,_,R,F}) -> % illegal
    {expr, [R | F]};
used_records({call,_,{atom,_,record},[A,{atom,_,Name}]}) ->
    {name, Name, A};
used_records({call,_,{atom,_,is_record},[A,{atom,_,Name}]}) ->
    {name, Name, A};
used_records({call,_,{remote,_,{atom,_,erlang},{atom,_,is_record}},
              [A,{atom,_,Name}]}) ->
    {name, Name, A};
used_records({call,_,{atom,_,record_info},[A,{atom,_,Name}]}) ->
    {name, Name, A};
used_records({call,Line,{tuple,_,[M,F]},As}) ->
    used_records({call,Line,{remote,Line,M,F},As});
used_records(T) when is_tuple(T) ->
    {expr, tuple_to_list(T)};
used_records(E) ->
    {expr, E}.

record_defs(RT, Names) ->
    lists:flatmap(fun(Name) -> ets:lookup(RT, Name)
                  end, Names).

do_expand_records([], E0) ->
    E0;
do_expand_records(UsedRecords, E0) ->
    RecordDefs = [Def || {_Name,Def} <- UsedRecords],
    L = 1,
    E = prep_rec(E0),
    Forms = RecordDefs ++ [{function,L,foo,0,[{clause,L,[],[],[E]}]}],
    [{function,L,foo,0,[{clause,L,[],[],[NE]}]}] =
        erl_expand_records:module(Forms, [strict_record_tests]),
    prep_rec(NE).

prep_rec({value,_CommandN,_V}=Value) ->
    %% erl_expand_records cannot handle the history expansion {value,_,_}.
    {atom,Value,ok};
prep_rec({atom,{value,_CommandN,_V}=Value,ok}) ->
    %% Undo the effect of the previous clause...
    Value;
prep_rec(T) when is_tuple(T) -> list_to_tuple(prep_rec(tuple_to_list(T)));
prep_rec([E | Es]) -> [prep_rec(E) | prep_rec(Es)];
prep_rec(E) -> E.

%% @doc replace the temporary variables with the actual value in a function
-spec var_to_val_in_fun( FunBody      :: string()
                       , RelativeLine :: integer()
                       , Bindings     :: edts_rte_server:binding()) -> string().
var_to_val_in_fun(FunBody, RelativeLine, Bindings) ->
  %% Parse function body to AbsForm
  {ok, FunBodyToken, _} = erl_scan:string(FunBody),
  {ok, AbsForm}         = erl_parse:parse_form(FunBodyToken),
  io:format("AbsForm:~p~n", [AbsForm]),
  %% Replace variable names with variables' value and
  %% combine the Token to function string again
  NewFunBody            = do_var_to_val_in_fun(AbsForm, RelativeLine, Bindings),
  %% io:format("New Body before flatten: ~p~n", [NewFunBody]),
  NewForm               = erl_pp:form(NewFunBody),
  lists:flatten(NewForm).

%% @doc replace variable names with values for a function
do_var_to_val_in_fun( {function, L, FuncName, Arity, Clauses0}
                    , RelativeLine, Bindings) ->
  Clauses = replace_var_with_val_in_fun_clauses( Clauses0, RelativeLine
                                               , Bindings),
  %% io:format("Replaced Clauses are:~p~n", [Clauses0]),
  {function, L, FuncName, Arity, Clauses}.

replace_var_with_val_in_fun_clauses(Clauses0, RelativeLine, Binding) ->
  {Clauses1, OtherClauses} =
    lists:splitwith(fun({clause,L,_ArgList0,_WhenList0,_Lines0}) ->
                        L < RelativeLine
                    end, Clauses0),
  do_replace_var_with_val_in_clauses(Clauses1, Binding) ++ OtherClauses.

do_replace_var_with_val_in_clauses([], _Binding)                            ->
  [];
do_replace_var_with_val_in_clauses( [{clause,L,ArgList0,WhenList0,Lines0}]
                                  , Binding)                                ->
  %% replace variables' name with values in argument list
  ArgList  = replace_var_with_val_args(ArgList0, Binding),
  %% replace variables' name with values in "when" list
  WhenList = replace_var_with_val_args(WhenList0, Binding),
  %% replace variables' name with values for each of the expressions
  Lines    = replace_var_with_val_in_expr(Lines0, Binding),
  [{clause,L,ArgList,WhenList,Lines}];
do_replace_var_with_val_in_clauses([H|T], Binding)                          ->
  [H|do_replace_var_with_val_in_clauses(T, Binding)].

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

%% @doc replace the variable in a list of expressions with its actual valuex
replace_var_with_val_in_exprs(Exprs, Bindings) ->
  lists:map(fun(Expr) ->
                replace_var_with_val_in_expr(Expr, Bindings)
            end, Exprs).

%% @doc replace the variable in the expression with its actual valuex
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
  Exprs = replace_var_with_val_in_exprs(Exprs0, Bindings),
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
replace_var_with_val_in_expr({op, L, Ops, LExpr0, RExpr0}, Bindings)      ->
  LExpr = replace_var_with_val_in_expr(LExpr0, Bindings),
  RExpr = replace_var_with_val_in_expr(RExpr0, Bindings),
  {op, L, Ops, LExpr, RExpr};
replace_var_with_val_in_expr( {call, L, {atom, L, F0}, ArgList0}
                            , Bindings)                                   ->
  F = replace_var_with_val_in_expr(F0, Bindings),
  {call, L, {atom, L, F}, replace_var_with_val_args(ArgList0, Bindings)};
replace_var_with_val_in_expr( {call, L, {remote, L, M0, F0}, Args0}
                            , Bindings)                                   ->
  M = replace_var_with_val_in_expr(M0, Bindings),
  F = replace_var_with_val_in_expr(F0, Bindings),
  {call, L, {remote, L, M, F}, replace_var_with_val_args(Args0, Bindings)};
replace_var_with_val_in_expr( {'case', L, CaseExpr0, Clauses0}
                            , Bindings)                                   ->
  CaseExpr = replace_var_with_val_in_expr(CaseExpr0, Bindings),
  Clauses  = replace_var_with_val_in_clauses(Clauses0, Bindings),
  {'case', L, CaseExpr, Clauses};
replace_var_with_val_in_expr( {string, _L, _Str} = String
                            , _Bindings)                                  ->
  String;
replace_var_with_val_in_expr( { 'try', L, Exprs0, PatternClauses0
                              , ExceptionClauses0, FinalExprs0}
                            , Bindings)                                   ->
  Exprs            = replace_var_with_val_in_exprs(Exprs0, Bindings),
  PatternClauses   = replace_var_with_val_in_clauses( PatternClauses0
                                                    , Bindings),
  ExceptionClauses = replace_var_with_val_in_clauses( ExceptionClauses0
                                                    , Bindings),
  FinalExprs       = replace_var_with_val_in_exprs( FinalExprs0
                                                  , Bindings),
  {'try', L, Exprs, PatternClauses, ExceptionClauses, FinalExprs};
replace_var_with_val_in_expr({lc, L, Expr0, GenExprs0}, Bindings)         ->
  Expr     = replace_var_with_val_in_expr(Expr0, Bindings),
  GenExprs = replace_var_with_val_in_exprs(GenExprs0, Bindings),
  {lc, L, Expr, GenExprs};
replace_var_with_val_in_expr({generate, L, ResExp, GenExp}, Bindings)     ->
  { generate, L, replace_var_with_val_in_expr(ResExp, Bindings)
  , replace_var_with_val_in_expr(GenExp, Bindings)};
replace_var_with_val_in_expr({'receive', L, Clauses0}, Bindings)          ->
  Clauses  = replace_var_with_val_in_clauses(Clauses0, Bindings),
  {'receive', L, Clauses};
replace_var_with_val_in_expr( {'receive', L, Clauses0, Int, Exprs0}
                            , Bindings)                                   ->
  Clauses  = replace_var_with_val_in_clauses(Clauses0, Bindings),
  Expr     = replace_var_with_val_in_exprs(Exprs0, Bindings),
  {'receive', L, Clauses, Int, Expr};
replace_var_with_val_in_expr( {record, _, _Name, _Fields} = Record
                            , _Bindings)                                  ->
  edts_rte_erlang:expand_records( edts_rte_server:record_table_name()
                                , Record);
replace_var_with_val_in_expr([Statement0|T], Bindings)                    ->
  Statement = replace_var_with_val_in_expr(Statement0, Bindings),
  [Statement | replace_var_with_val_in_expr(T, Bindings)];
replace_var_with_val_in_expr(Var, _Bindings)                              ->
  io:format("Var:~p~n", [Var]),
  error(crap).

replace_var_with_val({var, L, VariableName}, Bindings) ->
  Value = proplists:get_value(VariableName, Bindings),
  io:format("VarName:~p   L:~p    Val:~p~n", [VariableName, L, Value]),
  Val = do_replace(Value, L),
  io:format("replaced Var:~p~n", [Val]),
  Val;
replace_var_with_val(Other, _Bindings)                 ->
  Other.

do_replace(Value, L) ->
  ValStr           = lists:flatten(io_lib:format("~p.", [Value])),
  Tokens0          = get_tokens(ValStr),
  Tokens           = maybe_replace_pid(Tokens0, Value),
  {ok, [ValForm]}  = erl_parse:parse_exprs(Tokens),
  replace_line_num(ValForm, L).

get_tokens(ValStr) ->
  {ok, Tokens, _} = erl_scan:string(ValStr),
  Tokens.

%% pid is displayed as atom instead. coz it is not a valid erlang term
maybe_replace_pid(Tokens0, Value) ->
  case is_pid_tokens(Tokens0) of
    true  ->
      ValStr0 = lists:flatten(io_lib:format("{__pid__, ~p}", [Value])),
      io:format("pid token:~p~n", [Tokens0]),
      ValStr1 = re:replace(ValStr0, "\\.", ",", [{return, list}, global]),
      ValStr2 = re:replace(ValStr1, "\\<", "{", [{return, list}, global]),
      ValStr  = re:replace(ValStr2, "\\>", "}", [{return, list}, global]),
      get_tokens(ValStr++".");
    false ->
      Tokens0
  end.

is_pid_tokens(Tokens) ->
  [FirstElem | _] = Tokens,
  [{dot, _}, LastElem | _] = lists:reverse(Tokens),
  is_left_arrow(FirstElem) andalso is_right_arrow(LastElem).

is_left_arrow({Char, _}) when Char =:= '<' ->
  true;
is_left_arrow(_) ->
  false.

is_right_arrow({Char, _}) when Char =:= '>' ->
  true;
is_right_arrow(_) ->
  false.

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

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
