docstore
=====

`docstore` is an example-sketch OTP application that uses the key-process-value data structure from the book [Erlang and OTP in Action](https://www.google.co.uk/books/edition/Erlang_and_OTP_in_Action/1zgzEAAAQBAJ?hl=en&gbpv=1&dq=erlang+and+otp+in+action&printsec=frontcover), to help implement a document store. The document store architecture comes from another book - [Distributed Computing with Go: Practical concurrency and parallelism for Go applications](https://www.google.co.uk/books/edition/Distributed_Computing_with_Go/dulODwAAQBAJ?hl=en).

There are two OTP apps: `concierge` and `librarian`. The former is used to store documents into the library and lookup existing docs (with keys). The latter is used to lookup tokens, i.e. search for a word frequency across documents in the library.

Build
----
```shell
$ rebar3 compile
```

Example usage in a rebar3 shell
----
```Erlang
    % Add document
    {Title, Doc} = c_data:test_document().
    {"Autumn Haiku", "Golden leaves swirl down,\n Autumn breath paints the earth warm,\n Twilight hums its song."}   
    concierge:insert_doc(Title,Doc).
    concierge:lookup_doc(Title).

    % Get frequency count per document (Tokens need to be lower-cased and non stop-words)
    librarian:lookup("leaves").
    {ok,#{"Autumn Haiku" => {document,1,#{1 => [2]}}}}

    % Delete doc
    concierge:delete(Title).
```

    