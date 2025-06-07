-module(librarian_tests).

-include_lib("eunit/include/eunit.hrl").

-record(document, {count, locations}).

flush_store() ->
    %% 1) ensure the OTP app is running (starts your supervisors, 
    %%    including c_doc_element_sup)
    {ok, _} = application:ensure_all_started(concierge),
    {ok, _} = application:ensure_all_started(librarian),

    %% 2) drop any old ETS table
    catch ets:delete(c_store),
    %% 3) re-create it
    ok = c_store:init().

insert_and_lookup_tokens_test() ->
    flush_store(),
    librarian:delete("ocean"),

    % Insert sample tokens tied to realistic documents
    lists:foreach(
        fun({Doc, Line, Pos}) ->
            ok = librarian:insert_token("ocean", Doc, Line, Pos)
        end,
        [{"MarineBio", 1, 2}, {"MarineBio", 1, 3}, {"MarineBio", 2, 1},
         {"OceanicCurrents", 2, 4}, {"OceanicCurrents", 3, 1},
         {"DeepSeaEcology", 1, 5}]
    ),

    % Perform lookup
    {ok, DocMap} = librarian:lookup("ocean"),

    % Validate that keys exist and values are records
    ?assert(maps:is_key("MarineBio", DocMap)),
    ?assert(maps:is_key("OceanicCurrents", DocMap)),
    ?assert(maps:is_key("DeepSeaEcology", DocMap)),

    Marine = maps:get("MarineBio", DocMap),
    ?assertEqual(3, Marine#document.count),
    ?assert(maps:is_key(1, Marine#document.locations)),
    ?assert(maps:is_key(2, Marine#document.locations)).


multi_token_and_search_test() ->
    flush_store(),
    librarian:delete("breakfast"),
    librarian:delete("salad"),

    lists:foreach(
        fun({Token, Doc, Line, Pos}) ->
            ok = librarian:insert_token(Token, Doc, Line, Pos)
        end,
        [{"breakfast", "MorningEats", 1, 1},
         {"breakfast", "MorningEats", 1, 2},
         {"breakfast", "MorningEats", 2, 1},
         {"salad", "MorningEats", 2, 2},
         {"salad", "GreensGuide", 3, 1},
         {"salad", "GreensGuide", 3, 2}]
    ),

    {ok, BreakfastMap} = librarian:lookup("breakfast"),
    ?assert(maps:is_key("MorningEats", BreakfastMap)),

    {ok, SaladMap} = librarian:lookup("salad"),
    ?assert(maps:is_key("MorningEats", SaladMap)),
    ?assert(maps:is_key("GreensGuide", SaladMap)),

    {FScoreMap, FScores} = librarian:get_search_results(["breakfast", "salad"]),

    % Now use the correct token count keys
    ?assert(maps:is_key(4, FScoreMap)),  % MorningEats has 4 total tokens
    ?assert(maps:is_key(2, FScoreMap)),  % GreensGuide has 2

    ?assert(lists:member(4, FScores)),
    ?assert(lists:member(2, FScores)),
    ?assert(is_map(FScoreMap)).

delete_token_test() ->
    flush_store(),

    librarian:insert_token("ocean", "delete_me", 1, 1),
    {ok, MapBefore} = librarian:lookup("ocean"),
    ?assert(maps:is_key("delete_me", MapBefore)),
    ok = librarian:delete("ocean"),
    ?assertEqual({error, not_found}, librarian:lookup("ocean")).
