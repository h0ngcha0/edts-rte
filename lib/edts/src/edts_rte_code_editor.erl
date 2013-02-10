%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Module to manipulate code editors
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%_* Module declaration =======================================================
-module(edts_rte_code_editor).

%%%_* Exports ==================================================================
-export([ read/1
        , read_all/0
        , create_table/0
        , delete/1
        , delete_all/0
        , write/1
        , get_id/1
        , set_id/2
        , set_code/2
        , set_x/2
        , set_y/2
        , set_z/2
        ]).

-export([ to_json/1
        , from_json/1
        ]).

%%%_* Macro  ===================================================================
-define(ID_UNDEFINED, -1).
-define(TAB_NAME, code_editor).

%%%_* Records  =================================================================
-record(code_editor, { id   = ?ID_UNDEFINED
                     , code = ""
                     , x    = 0
                     , y    = 0
                     , z    = 0
         }).

%%%_* Type =====================================================================
-type code_editor() :: #code_editor{}.

%%%_* Code  ====================================================================
%% @doc Create the table for code editor.
%%      Function signature @see mnesia:create_table/2
create_table() ->
  mnesia:create_table( code_editor
                     , [ {attributes  , record_info(fields, code_editor)}
                       , {record_name , code_editor}
                       , {ram_copies, [node()]}]).

-spec read(Id :: integer()) -> code_editor().
read(Id) ->
  [CodeEditor] = mnesia:dirty_read(?TAB_NAME, Id),
  CodeEditor.

-spec write(CodeEditor :: code_editor()) -> ok.
write(CodeEditor) ->
  ok = mnesia:dirty_write(?TAB_NAME, CodeEditor).

-spec delete(Id :: integer()) -> ok.
delete(Id) ->
  ok = mnesia:dirty_delete(?TAB_NAME, Id).

-spec read_all() -> [code_editor()].
read_all() ->
  lists:foldl(fun(Key, Acc) ->
                  [read(Key)|Acc]
              end, [], mnesia:dirty_all_keys(?TAB_NAME)).

-spec delete_all() -> ok.
delete_all() ->
  lists:foreach(fun(Key) ->
                    ok = delete(Key)
                end, mnesia:dirty_all_keys(?TAB_NAME)).

get_id(#code_editor{id = Id}) ->
  Id.

set_id(#code_editor{} = CodeEditor, Id) ->
  CodeEditor#code_editor{id = Id}.

set_code(#code_editor{} = CodeEditor, Code) ->
  CodeEditor#code_editor{code = Code}.

set_x(#code_editor{} = CodeEditor, X) ->
  CodeEditor#code_editor{x = X}.

set_y(#code_editor{} = CodeEditor, Y) ->
  CodeEditor#code_editor{y = Y}.

set_z(#code_editor{} = CodeEditor, Z) ->
  CodeEditor#code_editor{z = Z}.

%% @doc convert article record to json binary
to_json([])                                    ->
  [];
to_json(CodeEditors) when is_list(CodeEditors) ->
  mochijson2:encode(lists:map(fun to_json_struct/1, CodeEditors));
to_json(#code_editor{} = CodeEditor)           ->
  mochijson2:encode(to_json_struct(CodeEditor)).

to_json_struct(#code_editor{} = CodeEditor) ->
  { struct
  , lists:zip( record_info(fields, code_editor)
             , tl(tuple_to_list(CodeEditor)))}.

from_json(CodeEditorJson) ->
  {struct, CodeEditorFields} = mochijson2:decode(CodeEditorJson),
  populate_code_editor_record(CodeEditorFields).

%% @doc populates the article record based on the article json struct
%% @end
populate_code_editor_record(CodeEditorFields) ->
  lists:foldl(fun({Key, Val}, Article) ->
                  Fun = set_code_editor_val_fun(Key),
                  Fun(Article, Val)
              end, #code_editor{}, CodeEditorFields).

set_code_editor_val_fun(<<"id">>) ->
  fun set_id/2;
set_code_editor_val_fun(<<"code">>) ->
  fun set_code/2;
set_code_editor_val_fun(<<"x">>) ->
  fun set_x/2;
set_code_editor_val_fun(<<"y">>) ->
  fun set_y/2;
set_code_editor_val_fun(<<"z">>) ->
  fun set_z/2.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
