
-module(erlymatch_SUITE).

-export(
    [ all/0
    , t_record/1
    , t_equal/1
    , t_match_basic/1
    , t_match_tuple/1
    , t_match_list/1
    , t_match_map/1
    , t_match_record/1
    , t_mismatch_details/1
    ]).

-include("erlymatch.hrl").
-include("../src/erlymatch_private.hrl").

-record(a, {b, c, d, e, f}).
-record(b, {c, d, e, f}).
-record(x, {y :: integer(),
            z :: string()}).

all() ->
    [ t_record
    , t_equal
    , t_match_basic
    , t_match_tuple
    , t_match_list
    , t_match_map
    , t_match_record
    , t_mismatch_details
    ].

%%% ----------------------------------------------------------------------------
%%% test reading record fields
%%% ----------------------------------------------------------------------------
t_record(Conf) when is_list(Conf) ->
    ?assertEqual(record_info(fields, a), ?rec_fields(a)),
    ?assertEqual(record_info(fields, b), ?rec_fields(b)),
    ?assertEqual(record_info(fields, x), ?rec_fields(x)),
    ?assertEqual(undefined, ?rec_fields(rec_not_defined)),
    ok.

%%% ----------------------------------------------------------------------------
%%% Test equal macro
%%% ----------------------------------------------------------------------------
t_equal(Conf) when is_list(Conf) ->
    {mismatch, _} = (catch ?assertEqual('_', {a})),
    ?assertEqual([or_b], [or_b]),
    ?assertEqual("or_whatsoever", "or_whatsoever"),
    ok.

%%% ----------------------------------------------------------------------------
%%% Test match macro, basic tests
%%% ----------------------------------------------------------------------------
t_match_basic(Conf) when is_list(Conf) ->
    ?assertMatch(_, b),
    [{a, b}] = mismatch_details(catch ?assertMatch(a, b)),

    ok = ?assertMatch(_, "stirng"),
    [{"a", "b"}] = mismatch_details(catch ?assertMatch("a", "b")),

    ok = ?assertMatch(_, {a, b}),
    [{a, b}, {b, a}] = mismatch_details(catch ?assertMatch({a, b}, {b, a})),

    ok = ?assertMatch('_', whatever),
    [{'_x', whatever}] = mismatch_details(catch ?assertMatch('_x', whatever)),
    ok.

%%% ----------------------------------------------------------------------------
%%% test match macro, tuples
%%% ----------------------------------------------------------------------------
t_match_tuple(Conf) when is_list(Conf) ->
    ?assertMatch({}, {}),
    [{{}, {a}}] = mismatch_details(catch ?assertMatch({}, {a})),
    [{{a}, {}}] = mismatch_details(catch ?assertMatch({a}, {})),
    [{{a, b}, {a}}] = mismatch_details(catch ?assertMatch({a, b}, {a})),

    ?assertMatch({_}, {b}),
    [{a, b}] = mismatch_details(catch ?assertMatch({a}, {b})),
    ok.

%%% ----------------------------------------------------------------------------
%%% test match macro, lists
%%% ----------------------------------------------------------------------------
t_match_list(Conf) when is_list(Conf) ->
    ?assertMatch([_], [a]),
    ?assertMatch([_ | _], [a]),
    [{a, b}] = mismatch_details(catch ?assertMatch([a | _], [b, b, c])),
    [{['_'], []}] = mismatch_details(catch ?assertMatch([_, _ | _], [a])),
    [{[], [b]}] = mismatch_details(catch ?assertMatch([a], [a, b])),
    [{a, b}, {b, c}, {c, d}] =
        mismatch_details(catch ?assertMatch([a, b, c], [b, c,d ])),
    ok.

t_match_map(Conf) when is_list(Conf) ->
    ok = ?assertMatch(#{}, #{}),
    [{c, b}] = mismatch_details(catch ?assertEqual(#{a => c}, #{a => b})),
    [{c, b}] = mismatch_details(catch ?assertEqual(#{a => #{key => c}}, #{a => #{ key => b}})).

%%% ----------------------------------------------------------------------------
%%% test match macro, records
%%% ----------------------------------------------------------------------------
t_match_record(Conf) when is_list(Conf) ->
    ?assertMatch(#a{}, #a{b = hasvalue}),
    ?assertMatch(#a{b='_'}, #a{b = hasvalue}),
    ?assertMatch(#a{b='_',c='_',d='_',e='_',f='_'}, #a{b = hasvalue}),
    [{hsavalue, hasvalue}] =
        mismatch_details(catch ?assertMatch(#a{b = hsavalue}, #a{b = hasvalue})),
    A = #a{b = b, c = c, d = d, e = e, f = f},
    B = #b{c = c, d = d, e = e, f = f},
    [{{a, b, c, d, e, f}, {b, c, d, e, f}}] =
        mismatch_details(catch ?assertMatch(A, B)),

    ?assertMatch(#a{b = _}, #a{b = hasvalue}),
    [{"something", "somethingelse"}] =
        mismatch_details(catch ?assertMatch([#a{}, "something"],
                                            [#a{d = value}, "somethingelse"])),
    ok.

%%% ----------------------------------------------------------------------------
%%% Test match macro some more
%%% ----------------------------------------------------------------------------
t_mismatch_details(Conf) when is_list(Conf) ->
    ?assertMatch([a, b | _], [a, b, c, d]),
    X = something,
    Line = ?LINE, Mismatch = (catch ?assertMatch(X, somethingelse)),
    ExpectedMismatch = { mismatch
                       , { ?MODULE
                         , Line
                         , { details
                           , value
                           , mismatch
                           , {something, somethingelse}
                           }
                         }
                       },
    ?assertEqual(ExpectedMismatch, Mismatch),
    ok.

%%% ----------------------------------------------------------------------------
%%% get mismatch details
%%% ----------------------------------------------------------------------------
mismatch_details({mismatch, {?MODULE, _Line, Details}}) ->
    mismatch_details([Details]);
mismatch_details([{_, _Type, match, _} | Rest]) ->
    mismatch_details(Rest);
mismatch_details([{_, value, mismatch, {Exp, Got}} | Rest]) ->
    [{Exp, Got} | mismatch_details(Rest)];
mismatch_details([{_, record, mismatch, [_RecordName | SubDetails]} | Rest]) ->
    mismatch_details(SubDetails) ++ mismatch_details(Rest);
mismatch_details([{_, _Type, mismatch, SubDetails} | Rest]) ->
    mismatch_details(SubDetails) ++ mismatch_details(Rest);
mismatch_details([]) ->
    [].

