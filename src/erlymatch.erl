
-module(erlymatch).

-export([ run/5
        , print/2
        ]).

-include("erlymatch_private.hrl").

-define(undef, undefined).
-define(ind_width, 4).

%% -----------------------------------------------------------------------------
%% run(Module, Line, Expected, Got, Options) -> ok | MismatchFormat
%%
%% Options = [{print, raw | all | lite}]
%% Format  = {Tag, Type, match | mismatch, Var}
%% Tag     = atom() | integer()
%% Type    = record | tuple | list | value
%% Var     = Expected                   |
%%           {Expected, Got}            |
%%           [record_name() | [Format]] |
%%           [Format]
%% -----------------------------------------------------------------------------
run(_Module, _Line, Value, Value, _Options) ->
    ok;
run(Module, Line, ExpectedValue, RealValue, Options) ->
    Format = format_match(Module, _RootTag = details, ExpectedValue, RealValue),
    Result = {Module, Line, Format},
    case [Opt || {print, Opt} <- Options] of
        [] ->
            do_nothing;
        PrintOptions ->
            print(Result, PrintOptions)
    end,
    throw({mismatch, Result}).

%% -----------------------------------------------------------------------------
%% print format
%% Options = [raw | all | lite]
%% -----------------------------------------------------------------------------
print({Module, Line, Format}, Options) ->
    fp("Match failed in module '~p' at line ~p:~n", [Module, Line]),
    print_match_format(Format, Options).

%% -----------------------------------------------------------------------------
%% format match status of each element
%% -----------------------------------------------------------------------------
format_match(Mod, Tag, A, B) when is_list(A), is_list(B) ->
    format_list_match(Mod, Tag, A, B);
format_match(Mod, Tag, A, B) when is_tuple(A), is_tuple(B) ->
    format_tuple_match(Mod, Tag, A, B);
format_match(Mod, Tag, A, B) when is_map(A), is_map(B) ->
    AKeys = lists:sort(maps:keys(A)),
    BKeys = lists:sort(maps:keys(B)),
    case AKeys =/= BKeys of
        true ->
            {Tag, map_keys, mismatch, {AKeys, BKeys}};
        false ->
            Sub = format_map_match(Mod, AKeys, A, B),
            {Tag, map, match_result(A, B), Sub}
    end;
format_match(_Mod, Tag, A, B) ->
    format_prim_match(Tag, A, B).


%% -----------------------------------------------------------------------------
%% format atomic match
%% -----------------------------------------------------------------------------
format_prim_match(Tag, A, B) ->
    case A == B of
        true ->
            {Tag, value, match, A};
        false ->
            {Tag, value, mismatch, {A, B}}
    end.

%% -----------------------------------------------------------------------------
%% match_Result(A, B) -> match | mismatch
%% -----------------------------------------------------------------------------
match_result(A, A) -> match;
match_result(_, _) -> mismatch.

%% -----------------------------------------------------------------------------
%% format list match
%% -----------------------------------------------------------------------------
format_list_match(Mod, Tag, A, B) ->
    case is_printable(A) andalso is_printable(B) of
        true ->
            format_prim_match(Tag, A, B);
        false ->
            Tags = lists:seq(1, length(A)),
            Sub = format_list_match_loop(Mod, Tags, A, B),
            {Tag, list, match_result(A, B), Sub}
    end.

format_list_match_loop(_Mod, _Tags, [], []) ->
    [];
format_list_match_loop(_Mod, _Tags, A, B) when A == [] orelse B == [] ->
    [{'LIST TAIL', value, mismatch, {A, B}}];
format_list_match_loop(Mod, [Tag | Tags], [H1 | T1], [H2 | T2]) ->
    [format_match(Mod, Tag, H1, H2)| format_list_match_loop(Mod, Tags, T1, T2)].

format_map_match(Mod, Keys, A, B) ->
    [format_match(Mod, Key, maps:get(Key, A), maps:get(Key, B)) || Key <- Keys].

%% -----------------------------------------------------------------------------
%% format tuple match
%% -----------------------------------------------------------------------------
format_tuple_match(_Mod, Tag, A, B) when size(A) /= size(B) ->
    {Tag, value, mismatch, {A, B}};
format_tuple_match(Mod, Tag, A, B) ->
    [HA | TA] = tuple_to_list(A),
    [HB | TB] = tuple_to_list(B),
    case Mod:?rec_fields(HA) of
        Fields when is_list(Fields) andalso HA == HB ->
            Sub = format_list_match_loop(Mod, Fields, TA, TB),
            {Tag, record, match_result(A, B), [HA | Sub]};
        _ ->
            Tags = lists:seq(1, size(A)),
            Sub = format_list_match_loop(Mod, Tags, [HA | TA], [HB | TB]),
            {Tag, tuple, match_result(A, B), Sub}
    end.

%% -----------------------------------------------------------------------------
%% get the sub format if it's record tuple or list
%% -----------------------------------------------------------------------------
sub_format(value, _Var) -> [];
sub_format(record, [_RecordName | Sub]) -> Sub;
sub_format(Type, Sub) when Type =:= list;
                           Type =:= tuple;
                           Type =:= map ->
    Sub.

%% -----------------------------------------------------------------------------
%% get the indention padding width
%% -----------------------------------------------------------------------------
padding_width(Format) ->
    padding_width(Format, 0, 0).

padding_width([], _Depth, Width) ->
    Width;
padding_width([Format | Rest], Depth, Width) ->
    Result = padding_width(Format, Depth, Width),
    padding_width(Rest, Depth, Result);
padding_width({Tag, Type, _MatchResult, Var}, Depth, Width) ->
    Result = case length(term2string(Tag)) - Depth * ?ind_width of
        TmpWidth when TmpWidth > 0 ->
            erlang:max(TmpWidth, Width);
        _ ->
            Width
    end,
    padding_width(sub_format(Type, Var), Depth+1, Result).

%% -----------------------------------------------------------------------------
%% print match format
%% -----------------------------------------------------------------------------
print_match_format(_Format, []) ->
    ok; %% no print option, ignore
print_match_format(Format, [raw | _]) ->
    io:format("~p~n", [Format]);
print_match_format(Format, Options) ->
    Width = padding_width(Format) + 2,
    print_match_format(Format, hd(Options), Width, 0).

print_match_format([], _Option, _Width, _Depth) ->
    ok;
print_match_format([Format | Rest], Option, Width, Depth) ->
    print_match_format(Format, Option, Width, Depth),
    print_match_format(Rest, Option, Width, Depth);
print_match_format({Tag, Type, MatchResult, Var}, Option, Width, Depth) ->
    TagStr = make_tag_str(Width + Depth * ?ind_width, Tag),
    VarStr = case Type of
        value when MatchResult == match ->
            term2string(Var);
        value when MatchResult == mismatch ->
            {A, B} = Var,
            "EXPECT = " ++ term2string(A) ++ "\n" ++
            lists:duplicate(length(TagStr) + 2, $\s) ++
            "   GOT = " ++ term2string(B);
        list ->
            "[...]";
        map ->
            "#{...}";
        tuple ->
            "{...}";
        record ->
            "#" ++ term2string(hd(Var)) ++ "{...}"
    end,
    case Option == all orelse MatchResult == mismatch of
        true ->
            fp(TagStr ++ ": " ++ VarStr ++ "\n");
        false ->
            do_nothing
    end,
    print_match_format(sub_format(Type, Var), Option, Width, Depth+1).

%% -----------------------------------------------------------------------------
%% Width = 4, Tag = "abc" -> " abc"
%% -----------------------------------------------------------------------------
make_tag_str(Width, Tag) when not is_list(Tag) ->
    make_tag_str(Width, term2string(Tag));
make_tag_str(Width, TagStr) ->
    lists:duplicate(Width - length(TagStr), $\s) ++ TagStr.

%% -----------------------------------------------------------------------------
%% check if list is printabel
%% -----------------------------------------------------------------------------
is_printable(L) when is_list(L) ->
    io_lib:printable_unicode_list(L).

%% -----------------------------------------------------------------------------
%% function: term2string(term()) -> string()
%%
%% convert term to string
%% -----------------------------------------------------------------------------
term2string(T) ->
  lists:flatten(io_lib:format("~10000p", [T])).
  %% re:replace(Str, "\\n", "\\\\n", [global, {return, list}]).

%% -----------------------------------------------------------------------------
%% formated print
%% -----------------------------------------------------------------------------
fp(String) ->
    io:put_chars(String).

fp(FormatStr, ArgList) ->
    fp(io_lib:format(FormatStr, ArgList)).

