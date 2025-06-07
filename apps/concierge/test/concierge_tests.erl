%% test/concierge_tests.erl
-module(concierge_tests).
-include_lib("eunit/include/eunit.hrl").

%%---------------------------------------------------------------------- 
%% Helper: start app & clear any existing state between tests 
%%---------------------------------------------------------------------- 
flush_store() ->
    %% 1) ensure the OTP app is running (starts your supervisors, 
    %%    including c_doc_element_sup)
    {ok, _} = application:ensure_all_started(concierge),
    {ok, _} = application:ensure_all_started(librarian),

    %% 2) drop any old ETS table
    catch ets:delete(c_store),
    %% 3) re-create it
    ok = c_store:init().
  
%%---------------------------------------------------------------------- 
%% 1) insert_doc / lookup_doc / delete 
%%---------------------------------------------------------------------- 
insert_lookup_delete_test() ->
    flush_store(),
    {Title, Doc} = c_data:test_document(),

    %% insert and lookup
    ok = concierge:insert_doc(Title, Doc),
    {ok, Fetched} = concierge:lookup_doc(Title),
    ?assertEqual(Doc, Fetched),

    %% delete and ensure it's gone
    ok = concierge:delete(Title),
    %% needs to wait due to async calls 
    timer:sleep(round(timer:seconds(rand:uniform()))),
    {error, not_found} = concierge:lookup_doc(Title).

%%---------------------------------------------------------------------- 
%% 2) readlines/1: write a temp file and read it back 
%%---------------------------------------------------------------------- 
readlines_file_test() ->
    flush_store(),
    Filename = filename:join(["/tmp", "eunit_test.txt"]),
    Lines = ["first line\n", "second line\n", "third line"],
    ok = file:write_file(Filename, lists:flatten(Lines)),

    Bins = concierge:readlines(Filename),
    ExpectedBins = [<<"first line">>, <<"second line">>, <<"third line">>],
    ?assertEqual(ExpectedBins, Bins),

    file:delete(Filename).

%%---------------------------------------------------------------------- 
%% 3) derive_token/1: punctuation, lower-case, stopwords 
%%---------------------------------------------------------------------- 
derive_token_test() ->
    flush_store(),
    [
      fun() ->
        ?assertEqual({ok, "hello"}, concierge:derive_token("HeLLo!"))
      end,
      fun() ->
        ?assertEqual({ok, "e2e"}, concierge:derive_token("E2E,"))
      end,
      fun() ->
        ?assertEqual({ok, "co-op"}, concierge:derive_token("Co-Op."))
      end,
      fun() ->
        ?assertMatch({error, is_token_word}, concierge:derive_token("the"))
      end
    ].

%%---------------------------------------------------------------------- 
%% 4) index_doc/2 & token insertion via librarian 
%%---------------------------------------------------------------------- 
index_doc_integration_test() ->
    flush_store(),
    Title = <<"TestDoc">>,
    Doc = "foo bar\nbaz foo",

    ok = concierge:insert_doc(Title, Doc),

    %% lookup the token "foo"
    {ok, #{Title := {document, 2, LineMap}}} = librarian:lookup("foo"),

    ?assertEqual([1], maps:get(1, LineMap)),
    ?assertEqual([2], maps:get(2, LineMap)).
