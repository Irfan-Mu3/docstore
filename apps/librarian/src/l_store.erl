-module(l_store).

-export([init/0, insert/2, delete/1, lookup/1, lookup_reverse/1]).

-define(TABLE_ID, ?MODULE).

init() ->
  ets:new(?TABLE_ID, [public, named_table]),
  ok.

insert(Key, Pid) ->
  ets:insert(?TABLE_ID, {Key, Pid}).

lookup(Key) ->
  case ets:lookup(?TABLE_ID, Key) of
    [{Key, Pid}] -> {ok, Pid};
    [] -> {error, not_found}
  end.

lookup_reverse(Pid) ->
  case ets:lookup(?TABLE_ID, Pid) of
    [{Key, Pid}] -> {ok, Key};
    [] -> {error, not_found}
  end.

% Whilst, there's only one entry, we don't have indexes, and have to search for the Pid associated with the Value
delete(Pid) ->
  ets:match_delete(?TABLE_ID, {'_', Pid}).



