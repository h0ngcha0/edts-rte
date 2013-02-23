-module(edts_parse_bindings).

-export([replace_function_body_with_debug_bingding/1]).

replace_function_body_with_debug_bingding(FunctionBody) ->
    %% Parse function body to AbsForm
    case erl_scan:string(FunctionBody) of
        {ok,FunctionBodyToken, _} -> FunctionBodyToken;
        _ -> io:format("Error happens in scaning function body"),
             FunctionBodyToken = {}
    end,

    case erl_parse:parse_form(FunctionBodyToken) of
        {ok, FunctionBodyAbsForm} -> FunctionBodyAbsForm;
        _ -> io:format("Error happens in parsing to Abs form")
    end,
    
    %% Replace variable names with variables' value and combine the Token to function string again
    NewFunctionBody = replace_variablename_with_value_function(FunctionBody),
    lists:flatten(erl_pp:form(NewFunctionBody)).

replace_variablename_with_value_function({function, L, FuncName, Arity, Clauses}) ->
    {function, L, FuncName, Arity, replace_variablename_with_value_clauses(Clauses)}.

replace_variablename_with_value_clauses([]) ->[];
replace_variablename_with_value_clauses([{clause,L,ArgList0,[],Lines0}|T]) ->
    %% replace variables' name with values in argument list
    ArgList = replace_variablename_with_value_arguments(ArgList0),
    %% replace variables' name with values for each function line
    Lines = replace_variablename_with_value_statements(Lines0),
    [{clause,L,ArgList,[],Lines} | replace_variablename_with_value_clauses(T)].

replace_variablename_with_value_arguments([])->[];
replace_variablename_with_value_arguments([VarExpr0|T]) ->
    VarExpr = replace_variablename_with_value(VarExpr0),
    [VarExpr | replace_variablename_with_value_arguments(T)].

replace_variablename_with_value_statements([]) -> [];
replace_variablename_with_value_statements({match,L,LExpr0,RExpr0}) -> 
    LExpr = replace_variablename_with_value_operations(LExpr0),
    RExpr = replace_variablename_with_value_operations(RExpr0),
    {match,L,LExpr,RExpr};
replace_variablename_with_value_statements({var, _, _} = VarExpr) ->
    replace_variablename_with_value(VarExpr);
replace_variablename_with_value_statements([Statement0|T]) -> 
    Statement = replace_variablename_with_value_statements(Statement0),
    [Statement | replace_variablename_with_value_statements(T)].

replace_variablename_with_value_operations({var, _, _} = VarExpr) ->
    replace_variablename_with_value(VarExpr);
replace_variablename_with_value_operations({op, L, '+', LExpr0, RExpr0})->
    LExpr = replace_variablename_with_value_operations(LExpr0),
    RExpr = replace_variablename_with_value_operations(RExpr0),
    {op, L, '+', LExpr, RExpr};
replace_variablename_with_value_operations({op, L, '-', LExpr0, RExpr0})->
    LExpr = replace_variablename_with_value_operations(LExpr0),
    RExpr = replace_variablename_with_value_operations(RExpr0),
    {op, L, '-', LExpr, RExpr};
replace_variablename_with_value_operations({op, L, '*', LExpr0, RExpr0})->
    LExpr = replace_variablename_with_value_operations(LExpr0),
    RExpr = replace_variablename_with_value_operations(RExpr0),
    {op, L, '*', LExpr, RExpr};
replace_variablename_with_value_operations({op, L, '/', LExpr0, RExpr0})->
    LExpr = replace_variablename_with_value_operations(LExpr0),
    RExpr = replace_variablename_with_value_operations(RExpr0),
    {op, L, '/', LExpr, RExpr};
replace_variablename_with_value_operations({call, L, {atom, L, F}, ArgList0}) ->
    {call, L, {atom, L, F}, replace_variablename_with_value_arguments(ArgList0)}.

replace_variablename_with_value({var, L, VariableName}) ->
    Value = orddict:fetch(VariableName, get_bindings()),
    {var, L, Value};
replace_variablename_with_value(OtherStruct) ->
    OtherStruct.

%% bindings and function body are from edts
get_bindings() ->
    bindings.
