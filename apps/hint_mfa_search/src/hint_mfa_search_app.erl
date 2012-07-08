-module(hint_mfa_search_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    hint_mfa_search_sup:start_link().

stop(_State) ->
    ok.
