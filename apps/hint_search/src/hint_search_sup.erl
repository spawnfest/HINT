
-module(hint_search_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ChildSpecs = child_specs([plt_cache_server]),
    {ok, { {one_for_one, 5, 10}, ChildSpecs} }.


child_specs(Children) ->
  child_specs(Children, []).

child_specs([], Acc) ->
  Acc;
child_specs([H|T], Acc) ->
  %% A very dumb "default" child spec that 
  %% might work most of the time
  %% TODO: a more flexible solution or specify
  %%       children manually when needed
  Spec = { H
         , {H, start_link, [[]]}
         , permanent
         , brutal_kill
         , worker
         , [H]},
  child_specs(T, [Spec | Acc]).