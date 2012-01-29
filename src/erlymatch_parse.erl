
-module(erlymatch_parse).

-export([ parse_transform/2
        ]).

-include("erlymatch_private.hrl").

%% -----------------------------------------------------------------------------
%% parse transform
%% -----------------------------------------------------------------------------
parse_transform(Forms, _Options) ->
    {NewForms, Records} = scan(Forms, [], []),
    scan_functions(NewForms, Records).

%% -----------------------------------------------------------------------------
%% traverse through the abstraction code, find all the defined records
%% -----------------------------------------------------------------------------
scan([], Acc, Records) ->
    {lists:reverse(Acc), Records};

scan([F = {attribute, LINE, module, _Mod} | Rest], Acc, Records) ->
    scan(Rest,
         [{attribute, LINE, export, [{?rec_fields, 1}]}, F | Acc],
         Records);

scan([F = {attribute, _L, record, {Rec, Fields}} | Rest], Acc, Records) ->
    scan(Rest, [F | Acc], [{Rec, length(Fields)} | Records]);

scan([F = {eof, LINE} | Rest], Acc, Records) ->
    scan(Rest, [F, make_rec_fields_function(Records, LINE) | Acc], Records);

scan([F | Rest], Acc, Records) ->
    scan(Rest, [F | Acc], Records).


%% -----------------------------------------------------------------------------
%% make the secretly exported function ?rec_fields/1
%% -----------------------------------------------------------------------------
make_rec_fields_function(Records, L) ->
    Clauses =
    [
        {clause, L,
            [{atom, L, Name}], [],
            [
                {call, L,
                    {atom, L, record_info},
                    [{atom, L, fields}, {atom, L, Name}]
                }
            ]
         } || {Name, _Size} <- Records
    ]
    ++
    [ {clause, L, [{var, L, '_'}], [], [{atom, L, undefined}]} ],
    {function, L, ?rec_fields, 1, Clauses}.

%% -----------------------------------------------------------------------------
%% Find out the functions (because ?match always happens in functions)
%% -----------------------------------------------------------------------------
scan_functions([], _Records) ->
  [];
scan_functions([F = {function, _, _, _, _} | Rest], Records) ->
  [scan_patterns(F, Records) | scan_functions(Rest, Records)];
scan_functions([F | Rest], Records) ->
  [F | scan_functions(Rest, Records)].

%% -----------------------------------------------------------------------------
%% find the pattern blow, and translate it to variable
%% {tuple,_,[{atom,_,?parse_me},{string,_,PatternStr}]}
%% -----------------------------------------------------------------------------
scan_patterns({tuple, _, [{atom, _, ?parse_me},
                         {string, Line, PatternStr}]}, Records) ->
    translate(Line, PatternStr, Records);
scan_patterns(F, Records) when is_list(F) ->
    [scan_patterns(I, Records) || I <- F];
scan_patterns(F, Records) when is_tuple(F) ->
    list_to_tuple([scan_patterns(I, Records) || I <- tuple_to_list(F)]);
scan_patterns(F, _Records) ->
    F.

%% -----------------------------------------------------------------------------
%% translate pattern string to variable
%% -----------------------------------------------------------------------------
translate(Line, PatternStr, Records) ->
  {ok, Tokens, Line} = erl_scan:string(PatternStr ++ ".", Line),
  {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
  translate_vars(Expr, Records).

%% -----------------------------------------------------------------------------
%% translate {var, Line, '_'} to {atom, Line, '_'}
%% -----------------------------------------------------------------------------
translate_vars({record, L, Name, Fields}, AllRecords) ->
  NumberOfFields = proplists:get_value(Name, AllRecords),
  NewFields = case length(Fields) of
                N when N < NumberOfFields ->
                  %% Line number here might be wrong, but, who cares ...
                  Fields ++ [{record_field,L,{var,L,'_'},{atom,L,'_'}}];
                _ ->
                  Fields
              end,
  {record, L, Name, translate_record_fields(NewFields, AllRecords)};
translate_vars({var, Line, '_'}, _) ->
  {atom, Line, '_'};
translate_vars(E, AllRecords) when is_list(E) ->
  [translate_vars(I, AllRecords) || I <- E];
translate_vars(E, AllRecords) when is_tuple(E) ->
  list_to_tuple([translate_vars(I, AllRecords) || I <- tuple_to_list(E)]);
translate_vars(E, _) ->
  E.

%% -----------------------------------------------------------------------------
%% special handling for record fields due to the fact that {var,Line,'_'} can
%% be used as the record filed name
%% -----------------------------------------------------------------------------
translate_record_fields([], _Records) ->
  [];
translate_record_fields([{record_field, L, FieldName, FieldValue} | Rest], R)->
  [ {record_field, L, FieldName, translate_vars(FieldValue, R)} |
    translate_record_fields(Rest, R) ].

