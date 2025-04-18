-module(l_app).
-behavior(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  l_store:init(),
  case l_sup:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Other ->
      {error, Other}
  end.

stop(_State) -> ok.
