%%%=============================================================================
%%% @doc Some nice shell features
%%%
%%% @end
%%%=============================================================================
-module(user_default).

-compile(debug_info).

%%%_* Exports ==================================================================

-export([ c/1
        , c/2
        , lm/0
        ]).

%%%_* Includes =================================================================

%%%_* Definitions ==============================================================

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc
%% equivalent to c:c(M, []).
-spec c(module()) -> {ok, module()} | {error, term()}.
%%------------------------------------------------------------------------------
c(M) -> c(M, []).

%%------------------------------------------------------------------------------
%% @doc
%% Like c:c but looks in the entire code-path.
-spec c(module(), compile:options()) -> {ok, module()} | {error, term()}.
%%------------------------------------------------------------------------------
c(M, Opts) ->
  try
    case shell_default:c(M, Opts++[debug_info]) of
      error ->
        Source = proplists:get_value(source, M:module_info(compile)),
        OutDir = filename:dirname(code:which(M)),
        shell_default:c(Source, Opts ++ [{outdir,OutDir}]);
      O -> O
    end
  catch error: E -> E
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Reload all currently loaded modules that have been modified.
-spec lm() -> [module()].
%%------------------------------------------------------------------------------
lm() ->
  Modified = [M || {M, _} <-  code:all_loaded(), module_modified(M) =:= true],
  [c:l(M) || M <- Modified].

%%%_* Internal =================================================================

module_modified(Module) ->
  case code:is_loaded(Module) of
    {file, preloaded} -> false;
    {file, Path}      ->
      CompileOpts = erlang:get_module_info(Module, compile),
      CompileTime = proplists:get_value(time, CompileOpts),
      Src = proplists:get_value(source, CompileOpts),
      case lists:last(Src) of
        $. -> false;  % compiled from abstract forms, no file
        _  -> module_modified(Path, CompileTime, Src)
      end;
    _ -> false
  end.

module_modified(Path, PrevCompileTime, PrevSrc) ->
  case find_module_file(Path) of
    non_existing -> removed;
    ModPath ->
      case beam_lib:chunks(ModPath, ["CInf"]) of
        {ok, {_, [{_, CB}]}} ->
          CompileOpts = binary_to_term(CB),
          CompileTime = proplists:get_value(time, CompileOpts),
          Src         = proplists:get_value(source, CompileOpts),
          CompileTime =/= PrevCompileTime orelse Src =/= PrevSrc;
        _ ->
          false
      end
  end.

find_module_file(Path) ->
  %% maybe the path was changed?
  case file:read_file_info(Path) of
    {ok, _} -> Path;
    _       -> code:where_is_file(filename:basename(Path))
  end.

%%%_* Tests ====================================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
