-module(l_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  TokenSup = {l_token_sup, {l_token_sup, start_link, []},
                    permanent, 2000, worker, [l_token_sup]},
  Children = [TokenSup],
  RestartStrategy = {one_for_one, 4, 3600},
  {ok, {RestartStrategy, Children}}.

