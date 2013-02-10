%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Homepage for RTE web interface
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%_* Module declaration =======================================================
-module(edts_resource_rte_home).

%%%_* Exports =================================================================
-export([ allowed_methods/2
        , init/1
        , process_post/2
        , to_html/2
        ]).

%%%_* Include =================================================================
-include_lib("webmachine/include/webmachine.hrl").

%%%_* Code ====================================================================
init([]) ->
  {ok, undefined}.

allowed_methods(ReqData, State) ->
  {['GET', 'POST'], ReqData, State}.

process_post(ReqData, State) ->
  {true, ReqData, State}.

to_html(ReqData, State) ->
  AbsPath  = filename:join([ code:priv_dir(edts)
                           , "index.html"]),
  io:format("AbsPath:~p~n", [AbsPath]),
  {ok, Content} = file:read_file(AbsPath),
  %% {ok, Content} = index_dtl:render([]),
  {Content, ReqData, State}.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
