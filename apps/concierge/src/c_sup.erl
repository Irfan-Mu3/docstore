-module(c_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  DocElementsSup = {c_doc_element_sup, {c_doc_element_sup, start_link, []},
    permanent, 2000, worker, [c_doc_element_sup]},
  Children = [DocElementsSup],
  RestartStrategy = {one_for_one, 4, 3600},
  {ok, {RestartStrategy, Children}}.

