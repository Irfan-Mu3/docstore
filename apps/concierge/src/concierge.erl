 -module(concierge).

-export([insert_doc/2, lookup_doc/1, delete/1, readlines/1, index_doc/2]).

insert_doc(Title, Document) ->
  case c_store:lookup(Title) of
    {ok, Pid} ->
      c_doc_element:replace(Pid, Document),
      ok = librarian:remove_doc(Title);
    {error, _} ->
      {ok, Pid} = c_doc_element:create(Document),
      c_store:insert(Title, Pid)
  end,
  ok = index_doc(Title, Document).

index_doc(Title, Document) ->
  Lines = string:split(Document, ["\n"], all),
  erlang:display(Lines),
  ok = insert_lines(Title,Lines , 1).

insert_lines(_Title, [], _LineNum) -> ok;
insert_lines(Title, [Line | Lines], LineNum) ->
  maybe
    TrimmedLine = string:trim(Line),
    true ?=  TrimmedLine =/= [],
    Words = string:split(TrimmedLine, " ", all),
    erlang:display(Words),
    ok = insert_words(Title, Words, LineNum, 1)
  end,
  ok = insert_lines(Title, Lines, LineNum + 1).

insert_words(_Title, [], _LineNum, _LineIdx) -> ok;
insert_words(Title, [Word | Words], LineNum, LineIdx) ->
  maybe
    {ok, Token} ?= derive_token(Word),
    erlang:display(Token),
    librarian:insert_token(Token, Title , LineNum, LineIdx)
  end,
  ok = insert_words(Title, Words, LineNum, LineIdx + 1).

readlines(FileName) ->
  {ok, Data} = file:read_file(FileName),
  binary:split(Data, [<<"\n">>], [global]).

lookup_doc(Title) ->
  maybe
    {ok, Pid} ?= c_store:lookup(Title),
    {ok, Document} = c_doc_element:fetch(Pid),
    {ok, Document}
  else
    {error, not_found} -> {error, not_found}
  end.

delete(Title) ->
  case c_store:lookup(Title) of
    {ok, Pid} ->
      c_doc_element:delete(Pid),
      librarian:remove_doc(Title);
    {error, _Reason} ->
      ok
  end.

derive_token(Word) ->
  Token = string:to_lower(re:replace(Word, "[^a-zA-Z0-9-]", "", [global, {return, list}])),
  case lists:member(Token, c_data:stopwords()) of
    true -> {error, is_token_word};
    false -> {ok, Token}
  end.
