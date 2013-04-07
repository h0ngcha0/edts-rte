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
-module(edts_rte_record_manager).

%%%_* Exports =================================================================
-export([ expand_records/2
        , read_and_add_records/2
        ]).

%%%_* Includes =================================================================
-include_lib("kernel/include/file.hrl").

%%%_* API ======================================================================
read_and_add_records(Module, RT) ->
  read_and_add_records(Module, '_', [], [], RT).

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

expand_records(RT, E0) ->
  UsedRecords = used_record_defs(E0, RT),
  do_expand_records(UsedRecords, E0).

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

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
