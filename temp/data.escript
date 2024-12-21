% Test concierge works
{Title, Doc} = c_data:test_document().
concierge:insert_doc(Title,Doc).
concierge:lookup_doc(Title).


% Test deletion works
concierge:delete(Title).
concierge:lookup_doc(Title).

% Test we can add tokens and look them up
librarian:insert_token("ocean", "Bladibla", 1, 2).
librarian:insert_token("ocean", "Bladibla", 1, 4).
librarian:insert_token("ocean", "Bladibla", 1, 3).
librarian:insert_token("ocean", "Bladibla", 2, 2).
librarian:insert_token("ocean", "Bladibla", 2, 4).
librarian:insert_token("ocean", "Bladibla", 3, 3).

librarian:insert_token("ocean", "Bladibla2", 1, 2).
librarian:insert_token("ocean", "Bladibla2", 1, 4).
librarian:insert_token("ocean", "Bladibla2", 1, 3).
librarian:insert_token("ocean", "Bladibla2", 2, 2).
librarian:insert_token("ocean", "Bladibla2", 2, 4).
librarian:insert_token("ocean", "Bladibla2", 3, 3).

librarian:insert_token("ocean", "fghgfh", 1, 2).
librarian:insert_token("ocean", "fghgfh", 1, 4).
librarian:insert_token("ocean", "fghgfh", 1, 3).
librarian:insert_token("ocean", "fghgfh", 2, 2).
librarian:insert_token("ocean", "fghgfh", 2, 4).
librarian:insert_token("ocean", "fghgfh", 3, 3).

librarian:insert_token("ocean", "teriyaki", 1, 3).
librarian:insert_token("ocean", "teriyaki", 2, 2).
librarian:insert_token("ocean", "teriyaki", 2, 4).
librarian:insert_token("ocean", "teriyaki", 3, 3).
librarian:lookup("ocean").

librarian:insert_token("eggs", "Bladibla", 1, 5).
librarian:insert_token("eggs", "Bladibla", 1, 6).
librarian:insert_token("eggs", "Bladibla", 1, 7).
librarian:insert_token("eggs", "Bladibla", 2, 5).
librarian:insert_token("eggs", "Bladibla", 2, 6).
librarian:insert_token("eggs", "Bladibla", 4, 7).

librarian:insert_token("eggs", "salad", 2, 5).
librarian:insert_token("eggs", "salad", 2, 6).
librarian:insert_token("eggs", "salad", 4, 7).
librarian:insert_token("eggs", "salad", 4, 8).
librarian:lookup("eggs").

librarian:get_search_results(["ocean", "ocean"]).


% Test librarian:delete
librarian:delete("ocean").
librarian:lookup("ocean").

% Test librarian works
DC = #{"banana" =>
           #{"Book1" =>
                 #{"Count" => 1,
                   "Line" => "blablablalb",
                   "LineNum" => 3}}}.

maps:update("Book1",
            #{"Count" => 1,
              "Line" => "blablablalb",
              "LineNum" => 3},
            maps:get("banana", DC)).
        
        
#{"Book1" => #{"Count" => 1, "Locs" => #{"1" => []}}}.
