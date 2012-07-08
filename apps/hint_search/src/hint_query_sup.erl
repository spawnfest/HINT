-module(hint_query_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{hint_query_worker, {hint_query_worker, start_link, []},
            temporary, brutal_kill, worker, [hint_query_worker]}]}}.