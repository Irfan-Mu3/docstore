-module(l_token_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child() ->
  supervisor:start_child(?SERVER, []).

init([]) ->
  Element = {l_token, {l_token, start_link, []},
             temporary, brutal_kill, worker, [l_token]},
  Children = [Element],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.

