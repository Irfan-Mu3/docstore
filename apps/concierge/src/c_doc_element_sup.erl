-module(c_doc_element_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Document) ->
  supervisor:start_child(?SERVER, [Document]).

init([]) ->
  Element = {c_doc_element, {c_doc_element, start_link, []},
              temporary, brutal_kill, worker, [c_doc_element]},
  Children = [Element],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.

