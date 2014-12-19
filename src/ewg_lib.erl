-module(ewg_lib).
-compile([export_all]).

validate(Params, []) ->
    {[], Params};
validate(Params, Validations) ->
    validate(Params, Validations, [], Params).

validate(Params, [{Name, mandatory} | Rest], Results, Validated) ->
    validate(Params, [{Name, mandatory, []} | Rest], Results, Validated);
validate(Params, [{Name, optional} | Rest], Results, Validated) ->
    validate(Params, [{Name, optional, []} | Rest], Results, Validated);

validate(Params, [{Name, Validation} | Rest], Results, Validated) ->
    validate(Params, [{Name, optional, Validation} | Rest], Results, Validated);

validate(Params, [{Name, Presence, Validation} | Rest], Results, Validated) when is_list(Validation) andalso Validation =/= []->
    %% chained validation, convert to list of tuples
    validate(Params, [{Name, Presence, V} || V <- Validation] ++ Rest, Results, Validated);

validate(Params, [{Name, Presence, Validation} | Rest], Results, Validated) ->
    {NewValidated, NewResults} = case  {Presence, string:strip(plget(Name, Params, []))} of
        {mandatory, []} ->
            {Validated, Results ++ [{Name, "This parameter is mandatory"}]};
        {{mandatory, ErrorText}, []} ->
            {Validated, Results ++ [{Name, ErrorText}]};
        {optional, []} ->
            {plset(Name, [], Validated), Results};
        {{optional, Default}, []} ->
            {plset(Name, Default, Validated), Results};
        {_, Value}->
            case validator({Name, Value}, Validation, Validated) of
                {ok, NewValue} ->
                    {plset(Name, NewValue, Validated), Results};
                {error, ErrorText} ->
                    {Validated, Results ++ [{Name, ErrorText}]}
            end
    end,
    validate(Params, Rest, NewResults, NewValidated);

validate(Params, [], Results, Validated) ->
    %% If any validation failed then return the original request data so
    %% the form can be redisplayed with the values filled in.
    Failed = [{Name, proplists:get_all_values(Name, Results)} || Name <- proplists:get_keys(Results)],
    {Failed,  if Failed == [] -> Validated; true -> Params end}.

% Dummy Validator when a string has to be mandatory only with no further validations
validator({_Name, Value}, [], _V) -> {ok, Value};
validator(Param, {same_as, OName}, V) ->
    validator(Param, {same_as, OName, "This parameter should have the same value as " ++ OName}, V);
validator({_Name, Value}, {same_as, OName, Text}, V) ->
    case plget(OName, V) of
        Value -> {ok, Value};
        _Other -> {error, Text}
    end;

validator(Param, {not_same_as, OName}, V) ->
    validator(Param, {not_same_as, OName, "This parameter should NOT have the same value as " ++ OName}, V);
validator({_Name, Value}, {not_same_as, OName, Text}, V) ->
    case plget(OName, V) of
        Value -> {error, Text};
        _Other -> {ok, Value}
    end;

validator(Param, int, V) ->
    validator(Param, {int, "Invalid integer"}, V);
validator(Param, {int, Text}, V) ->
    validator(Param, {int, undefined, undefined, Text}, V);
validator(Param, {int, Min, Max}, V) ->
    validator(Param, {int, Min, Max, "Not an integer between " ++ tost(Min) ++ " and " ++ tost(Max)}, V);
validator({_Name, Value}, {int, _, _, _Text}, _) when Value=:="null"; Value=:="NULL" ->
    {ok, null};
validator({_Name, Value}, {int, Min, Max, Text}, _) ->
    case catch(list_to_integer(Value)) of
        IntVal when is_integer(IntVal),Min=:=undefined,Max=:=undefined ->
            {ok, IntVal};
        IntVal when is_integer(IntVal),IntVal>=Min,IntVal=<Max ->
            {ok, IntVal};
        _ ->
            {error, Text}
    end;

validator(Param, number, V) ->
    validator(Param, {number, "Not a number"}, V);
validator(Param, {number, Text}, V) ->
    validator(Param, {number, undefined, undefined, Text}, V);
validator(Param, {number, Min, Max}, V) ->
    validator(Param, {number, Min, Max, "Not a number between " ++ tost(Min) ++ " and " ++ tost(Max)}, V);
validator({_Name, Value}, {number, _, _, _Text}, _) when Value=:="null"; Value=:="NULL" ->
    {ok, null};
validator({_Name, Value}, {number, Min, Max, Text}, _) ->
    case catch(l2n(Value)) of
        Val when is_number(Val),Min=:=undefined,Max=:=undefined ->
            {ok, Val};
        Val when is_number(Val),Val>=Min,Val=<Max ->
            {ok, Val};
        _ ->
            {error, Text}
    end;

validator(Param, ip, V) ->
    validator(Param, {ip, "Invalid IP address format"}, V);
validator({_Name, Value}, {ip, Text}, _) ->
    case io_lib:fread("~d.~d.~d.~d", Value) of
        {ok, [A,B,C,D], []} ->
            case [Nr >= 0 andalso Nr <256 || Nr <- [A,B,C,D]] of
                [true, true, true, true] ->
                    {ok, {A,B,C,D}};
                _ ->
                    {error, Text}
            end;
        _ ->
            {error, Text}
    end;

%% This is actually only type conversion and will never fail, allow without error text
validator({_Name, Value}, 'atom', _) ->
    {ok, list_to_atom(Value)};
validator({_Name, Value}, {'atom', _Text}, _) ->
    {ok, list_to_atom(Value)};

validator({_Name, Value}, {member_atom, List, Text}, _) ->
    AtomVal = list_to_atom(Value),
    case lists:member(AtomVal, List) of
        false ->
            {error, Text};
        true ->
            {ok, AtomVal}
    end;

%% Shortcut to member_string
validator(Param, {member, List, Text}, V) ->
    validator(Param, {member_string, List, Text}, V);

validator({_Name, Value}, {member_string, List, Text}, _) ->
    case lists:member(Value, List) of
        false ->
            {error, Text};
        true ->
            {ok, Value}
    end;

validator(Param, regexstr, V) ->
    validator(Param, {regexstr, "Invalid a regular expression string"}, V);
validator({_Name, Value}, {regexstr, Text}, _) ->
    case catch re:run("1234", Value, [{capture, none}]) of
        match ->
            {ok, Value};
        nomatch ->
            {ok, Value};
        _ -> {error, Text}
    end;

validator(Param, term, V) ->
    validator(Param, {term, "Invalid Erlang term"}, V);
validator({_Name, Value}, {term, Text}, _) ->
    case catch parse_term(Value) of
        {ok, Term} ->
            {ok, Term};
        _ ->
            {error, Text}
    end;

validator({Name, Value}, {regex, Regex}, V) ->
    validator({Name, Value}, {regex, Regex, "Value [: " ++ Value ++ "] doesn't match regular expression " ++ Regex}, V);
validator({_Name, Value}, {regex, Regex, Text}, _) ->
    case catch re:run(Value, Regex, [{capture, none}]) of
        match ->
            {ok, Value};
        _ ->
            {error, Text}
    end;

%% Validate with custom callbacks, these should return {ok, NewValue} in case of success or {error, ErrorText} in case of error
validator(Param, Fun, V) when is_function(Fun) -> Fun(Param, V);
validator(Param, {mf, M, F}, V) -> apply(M, F, [Param, V]);
validator(Param, {mfa, M, F, A}, V) -> apply(M, F, [Param, V | A]).

%% automagical conversions from internal data to GUI
% undefined
tost(undefined) -> "";
% ip
tost({A, B, C, D}) ->
    lists:flatten(io_lib:format("~w.~w.~w.~w", [A, B, C, D]));
% int
tost(Val) when is_integer(Val) ->
    integer_to_list(Val);
% float
tost(Val) when is_float(Val) ->
    termtost(Val);
%    float_to_list(Val);

% atom
tost(Val) when is_atom(Val) ->
    atom_to_list(Val);

% tuple, convert as term
tost(Val) when is_tuple(Val) ->
    termtost(Val);

% keep others (assume string)
tost(Val) -> Val.

% convert terms
termtost(Val) ->
    lists:flatten(io_lib:format("~p", [Val])).

parse_term(String) ->
    {ok, Tokens, _} = erl_scan:string(String ++ "."),
    erl_parse:parse_term(Tokens).

%% simplistic list to number
l2n(L) ->
    case lists:member($., L) of
        true -> list_to_float(L);
        false -> list_to_integer(L)
    end.

plset(Key, Val, PL) ->
    lists:keystore(Key, 1, PL, {Key, Val}).

mplset([{Key, Val} | Rest], PL) ->
    mplset(Rest, plset(Key, Val, PL));
mplset([], PL) -> PL.

plget(Key, PL) ->
    plget(Key, PL, undefined).
plget(Key, PL, Default) ->
    % having undefined if no Key is fine
    case lists:keyfind(Key, 1, PL) of
        false -> Default;
        {Key, Val} -> Val
    end.

%% deep plget
dplget(Key, PL) -> dplget(Key, PL, undefined).
dplget(Key, PL, Default) when not is_list(Key) -> dplget([Key], PL, Default);
dplget([Key], PL, Default) -> proplists:get_value(Key, PL, Default);
dplget([Key | Rest], PL, Default) ->
    case proplists:get_value(Key, PL, Default) of
        undefined -> undefined;
        true -> true;
        Val when is_list(Val) ->
            case io_lib:char_list(Val) of
                true -> Default;
                _ -> dplget(Rest, Val, Default)
            end;
        _ -> Default
    end.

pldel(Key, PL) ->
    lists:keydelete(Key, 1, PL).

plmods(PL1, PL2) ->
    [{K, V, plget(K, PL2)} || {K, V} <- lists:subtract(PL1, PL2)].

% New proplist manipulation that support nested structures, will deprecate the ones above
% dive in to nested proplist
pl_getv(Key, PL) -> pl_getv(Key, PL, undefined).
pl_getv(Key, PL, Default) when not is_list(Key) -> pl_getv([Key], PL, Default);
pl_getv([Key], PL, Default) ->
    case lists:keyfind(Key, 1, PL) of
        {Key, Val} -> Val;
        _ -> Default
    end;
pl_getv([Key | Rest], PL, Default) ->
    case lists:keyfind(Key, 1, PL) of
        {Key, Val} when is_list(Val) ->
            case io_lib:char_list(Val) of
                true -> Default;
                _ -> pl_getv(Rest, Val, Default)
            end;
        _ ->
            Default
    end.

% update nested proplist
pl_setv(Key, Val, PL) when not is_list(Key) -> pl_setv([Key], Val, PL);
pl_setv([Key], Val, PL) -> lists:keystore(Key, 1, PL, {Key, Val});
pl_setv([Key | Rest], Val, PL) ->
    CurrentProp = case lists:keyfind(Key, 1, PL) of
        {Key, Prop} when is_list(Prop) ->
            case io_lib:char_list(Prop) of true -> []; _ -> Prop end;
        _ -> []
    end,
    NKey = case io_lib:char_list(Key) of true -> [Key]; _ -> Key end,
    pl_setv(NKey, pl_setv(Rest, Val, CurrentProp), PL).


% delete from nested proplist
pl_del(Key, PL) when not is_list(Key) -> pl_del([Key], PL);
pl_del([Key], PL) -> lists:keydelete(Key, 1, PL);
pl_del([Key | Rest], PL) ->
    case lists:keyfind(Key, 1, PL) of
        {Key, Val} when is_list(Val) ->
            NKey = case io_lib:char_list(Key) of true -> [Key]; _ -> Key end,
            pl_setv(NKey, pl_del(Rest, Val), PL);
        _ -> PL
    end.


id() ->
    tost(crypto:rand_uniform(0, 100000000000000)).

%% Date parsing and formatting

parse_date(DateStr) ->
    [D, M, Y, H, Mi] = [list_to_integer(I) || I <- string:tokens(DateStr, "/ :")],
    {{Y, M, D}, {H, Mi, 0}}.

pdate() ->
    {{Y, Mo, D}, {H, M, _S}} = calendar:local_time(),
    lists:flatten(io_lib:format("~2.10.0B/~2.10.0B/~4.10B ~2.10.0B:~2.10.0B", [D, Mo, Y, H, M])).
pdate({Y, Mo, D}) ->
    lists:flatten(io_lib:format("~2.10.0B/~2.10.0B/~4.10B", [D, Mo, Y]));
pdate({{Y, Mo, D}, {H, M, _S}}) ->
    lists:flatten(io_lib:format("~2.10.0B/~2.10.0B/~4.10B ~2.10.0B:~2.10.0B", [D, Mo, Y, H, M]));
pdate(Int) when is_integer(Int) ->
%% convert internal gregorian seconds to localtime
    pdate(
        calendar:universal_time_to_local_time(
            calendar:gregorian_seconds_to_datetime(Int)
        )
    ).

pdate_local(Int) when is_integer(Int) ->
%% convert internal gregorian seconds to localtime
    pdate(
        calendar:universal_time_to_local_time(
            calendar:gregorian_seconds_to_datetime(Int)
        )
    );
pdate_local(Date) ->
    pdate(calendar:universal_time_to_local_time(Date)).


dateadd(Amount, weeks) -> dateadd(Amount, week);
dateadd(Amount, week) ->  dateadd(Amount*7, day);
dateadd(Amount, days) ->  dateadd(Amount, day);
dateadd(Amount, day) ->   dateadd(Amount*24, hour);
dateadd(Amount, hours) -> dateadd(Amount, hour);
dateadd(Amount, hour) ->  dateadd(Amount*60, minute);
dateadd(Amount, minutes) -> dateadd(Amount, minute);
dateadd(Amount, minute) ->  dateadd(Amount*60).
dateadd(AmountSec) ->
    {{Y, Mo, D}, {H, M, _S}} = calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(calendar:local_time()) + AmountSec
    ),
    lists:flatten(io_lib:format("~2.10.0B/~2.10.0B/~4.10B ~2.10.0B:~2.10.0B", [D, Mo, Y, H, M])).

%% Generate password
%% 8 characters min, min 1 number and 1 capitals
pwgen() ->
    pwgen(
        %% Min, Max pw length
        6, 10,
        %% sets of characters to use to generate password
        %% [{MinNr, MaxNr, Chars}, ....]
        [
            {1, 3, "0123456789"},
            {1, 4, "ABCDEFGHIJKLMNOPQRSTUVWXYZ"},
            {1, 20, "abcdefghijklmnopqrstuvwxyz"}
        ]
    ).

pwgen(MinLength, MaxLength, Sets) ->
    Length = crypto:rand_uniform(MinLength, MaxLength+1),
    %% First make minimum requred set
    Req = lists:flatten([pwgen_req(Min, Chars) || {Min, _, Chars} <- Sets]),
    RemLength = Length - length(Req),
    if
        RemLength < 0 ->
            %% Can happen that min requred setnrs exceed max required length ...
            %% give some (not very) suggestive error message
            {error, condition_error};
        true ->
            pwgen_fill(pwgen_flatsets(Sets), RemLength, Req)
    end.

%% flatten the sets
pwgen_flatsets(Sets) ->
    pwgen_flatsets(Sets, 1, [], []).

pwgen_flatsets([{Min, Max, Chars} | RestSets], SeqNr, SetAcc, Acc) ->
    pwgen_flatsets(
        RestSets, SeqNr + 1, [{SeqNr, Max-Min} | SetAcc],
        Acc ++ [{SeqNr, C} || C <- Chars]
    );
pwgen_flatsets([], _, SetData, FlatSet) -> {SetData, FlatSet}.

pwgen_fill(_, 0, Acc) ->
    [C || {_, C} <- lists:sort([{crypto:rand_uniform(1, 1000), AC} || AC <- Acc])];
pwgen_fill({SetData, FlatSet}, Length, Acc) ->
    %% generate character and update set data
    {Set, Char} = lists:nth(crypto:rand_uniform(1, length(FlatSet) + 1), FlatSet),
    SetLimit = plget(Set, SetData),
    if SetLimit > 0 ->
        pwgen_fill(
            {plset(Set, SetLimit-1, SetData), FlatSet},
            Length-1,
            [Char | Acc]
        );
    true ->
        pwgen_fill(
            {pldel(Set, SetData), [{S, C} || {S, C} <- FlatSet, S =/= Set]},
            Length-1,
            [Char | Acc]
        )
    end.

pwgen_req(Nr, Chars) ->
    L = length(Chars),
    %% not excluding same char multiple times
    [lists:nth(crypto:rand_uniform(1, L + 1), Chars) || _ <- lists:seq(1, Nr)].

cookie_name() ->
    "ewebgui_session_" ++ atom_to_list(node()).


