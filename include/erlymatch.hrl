
-ifndef(ERLYMATCH_HRL_).
-define(ERLYMATCH_HRL_, true).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile({parse_transform, erlymatch_parse}).

-define( erlyequal(A, B),
         erlymatch:run(?MODULE, ?LINE, A, B, [{print, all}])
       ).

-define( erlyequal_opt(A, B, Opt),
         erlymatch:run(?MODULE, ?LINE, A, B, Opt)
       ).

-define( erlymatch(Pattern, Value),
         ( fun() ->
                case (Value) of
                Pattern ->
                    ok;
                __Value ->
                    %% ??Pattern is parsed into Variable by erlymatch_parse
                    __Parsed = {'$ERLYMATCH_PARSE_ME', ??Pattern},
                    __Fixed = erlymatch_ignore:fix(__Parsed, __Value),
                    %% let ?erlyequal report the mismatch details
                    ?erlyequal(__Fixed, __Value)
                end
            end
         )()
       ).

-ifdef(assertEqual).
-undef(assertEqual).
-define(assertEqual(Expected, Expr), ?erlyequal(Expected, Expr)).
-endif.

-ifdef(assertMatch).
-undef(assertMatch).
-define(assertMatch(Pattern, Expr), ?erlymatch(Pattern, Expr)).
-endif.

-endif.

