%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Webmachine resource to serve static contents
%%%      NOTE: this is just a workaround, there are better solutions for this
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =====================================================
-module(edts_resource_rte_static).

%%%_* Exports ================================================================
-export([ init/1
        , resource_exists/2
        , content_types_provided/2
        , generate_body/2
        ]).

%%%_* Include ================================================================
-include_lib("webmachine/include/webmachine.hrl").

%%%_* Record =================================================================
-record(ctx, { docroot    %% where our static files live
             , fullpath   %% path to file we'll send
             }).

%%%_* Code ===================================================================
init(DocRoot) ->
  {ok, #ctx{docroot=DocRoot}}.

resource_exists(RD, Ctx) ->
  Path = wrq:disp_path(RD),
  Rel  = filename:join([ P || P <- string:tokens(Path, "/"),
                              P /= ".."]),
  Abs  = filename:join([ code:priv_dir(edts)
                       , Ctx#ctx.docroot
                       , Rel]),
  {filelib:is_file(Abs), RD,Ctx#ctx{fullpath=Abs}}.

content_types_provided(RD, Ctx) ->
  Path = wrq:disp_path(RD),
  {[{webmachine_util:guess_mime(Path), generate_body}], RD, Ctx}.

generate_body(RD, Ctx) ->
  {ok, Data} = file:read_file(Ctx#ctx.fullpath),
  {Data, RD, Ctx}.

%%%_* Emacs ==================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
