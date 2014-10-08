-module(combine_samples).

-export([combine_samples/2]).

-define(DISTRIBUTION, 8).

combine_samples(Primary, Secondary) ->
    combine_samples(Primary, Secondary, [], 1, random:uniform(?DISTRIBUTION)).

combine_samples(Primary, Secondary, Result, Position, NextInsert) when Position == NextInsert, Secondary =/= [] ->
    [ PrimaryElement | NewPrimary ] = Primary,
    [ SecondaryElement | NewSecondary ] = Secondary,
    NewResult = lists:append(Result, [ PrimaryElement, SecondaryElement ]),
    NewPosition = Position + 1,
    NewNextInsert = NewPosition + random:uniform(?DISTRIBUTION),
    io:format("Calculated new next insert ~p.\n", [NewNextInsert]),
    combine_samples(NewPrimary, NewSecondary, NewResult, NewPosition, NewNextInsert);
combine_samples(Primary, Secondary, Result, _Position, _NextInsert) when Primary == [] ->
    lists:append(Result, Secondary);
combine_samples(Primary, Secondary, Result, Position, NextInsert) ->
    [ PrimaryElement | NewPrimary ] = Primary,
    NewResult = lists:append(Result, [ PrimaryElement ]),
    combine_samples(NewPrimary, Secondary, NewResult, Position + 1, NextInsert).

