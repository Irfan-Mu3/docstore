-module(l_store).

-export([init/0, insert/3, delete/2, lookup/2, lookup_reverse/2, list_table/1]).

-define(TOKEN_TABLE_ID, l_store_tokens).
-define(DOC_TABLE_ID, l_store_documents).

init() ->
  ets:new(?TOKEN_TABLE_ID, [public, named_table]),
  ets:new(?DOC_TABLE_ID, [public, named_table]),
  ok.

insert(token, Key, Pid) ->
  ets:insert(?TOKEN_TABLE_ID, {Key, Pid});
insert(document, Key, TokenPid) ->
  case ets:lookup(?DOC_TABLE_ID, Key) of
    [{Key, TokenPids}] -> ets:insert(?DOC_TABLE_ID, {Key, ordsets:add_element(TokenPid, TokenPids)});
    [] -> ets:insert(?DOC_TABLE_ID, {Key, [TokenPid]})
  end.

lookup(token, Key) ->
  case ets:lookup(?TOKEN_TABLE_ID, Key) of
    [{Key, Pid}] -> {ok, Pid};
    [] -> {error, not_found}
  end;
lookup(document, DocKey) ->
  case ets:lookup(?DOC_TABLE_ID, DocKey) of
    [{DocKey, TokenPids}] -> {ok, TokenPids};
    [] -> {error, not_found}
  end.

lookup_reverse(token, Pid) ->
  case ets:match(?TOKEN_TABLE_ID, {'$1', Pid}) of
    [[Key]] -> {ok, Key};
    [[]] -> {error, not_found}
  end.

% Whilst, there's only one entry, we don't have indexes, and have to search for the Pid associated with the Value
delete(token, Pid) ->
  ets:match_delete(?TOKEN_TABLE_ID, {'_', Pid});
delete(document, DocKey) ->
  ets:delete(?DOC_TABLE_ID, DocKey).

list_table(token) ->
  ets:tab2list(?TOKEN_TABLE_ID);
list_table(document) ->
  ets:tab2list(?DOC_TABLE_ID).