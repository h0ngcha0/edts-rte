%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Editors Resource
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%_* Module declaration =======================================================
-module(edts_resource_rte_editors).

%%%_* Exports =================================================================
-export([ allowed_methods/2
        , content_types_accepted/2
        , content_types_provided/2
        , delete_resource/2
        , init/1
        , process_post/2
        , to_json/2
        ]).

%%%_* Include =================================================================
-include_lib("webmachine/include/webmachine.hrl").

%%%_* Code ====================================================================
init([]) ->
  ct:pal("in editors resource init:"),
  {ok, undefined}.

%% This should take a list of the functions that we wanna display and display
%% it on the web page
allowed_methods(ReqData, State) ->
  {['GET', 'POST', 'PUT', 'DELETE'], ReqData, State}.

content_types_provided(RD, Ctx) ->
  {[{"application/json", to_json}], RD, Ctx}.

content_types_accepted(RD, Ctx) ->
  CT = case wrq:get_req_header("content-type", RD) of
         undefined -> "application/json";
         X         -> X
       end,
  {MT, _Params} = webmachine_util:media_type_to_detail(CT),
  {[{MT, process_post}], RD, Ctx}.

to_json(RD, Ctx) ->
  Result = edts_rte_code_editor:to_json(edts_rte_code_editor:read_all()),
  ct:pal("Editors:~p", [list_to_binary(Result)]),
  {Result, RD, Ctx}.

process_post(ReqData, State) ->
  %% Assuming that only one editor could be added at a time.
  %% TODO: add support for more than one editors
  Editor = edts_rte_code_editor:from_json(wrq:req_body(ReqData)),
  ok     = edts_rte_code_editor:write(Editor),
  {true, ReqData, State}.

delete_resource(ReqData, State) ->
  [Id] = wrq:path_tokens(ReqData),
  ct:pal("delete editor, id:~p~n", [Id]),
  ok   = edts_rte_code_editor:delete(list_to_binary(Id)),
  {true, ReqData, State}.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
