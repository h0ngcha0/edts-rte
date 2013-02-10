%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Resource for displaying the functions
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%_* Module declaration =======================================================
-module(edts_resource_rte_functions).

%%%_* Exports =================================================================
-export([ allowed_methods/2
        , content_types_provided/2
        , init/1
        , process_post/2
        , to_json/2
        ]).

%%%_* Include =================================================================
-include_lib("webmachine/include/webmachine.hrl").

%%%_* Code ====================================================================
init([]) ->
  ct:pal("in funcitons resource init"),
  {ok, undefined}.

%% This should take a list of the functions that we wanna display and display
%% it on the web page
allowed_methods(ReqData, State) ->
  {['GET', 'POST'], ReqData, State}.

content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json}], RD, Ctx}.

to_json(RD, Ctx) ->
  ct:pal("in get for functions_resource"),
  Result = "{id: 0, code:bla}",
  {Result, RD, Ctx}.

process_post(ReqData, State) ->
  ct:pal("in post for functions_resource"),
  {true, ReqData, State}.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
