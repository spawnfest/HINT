%%
%% Module for dev-mode only, will later be replaced with plt-based data feed
%%
-module(hint_mfa_info_to_ets).

-export([
		import/1
	]).

-include_lib("eunit/include/eunit.hrl").

-type tab() :: ets:tab().
%-type match_spec() :: ets:match_spec().

%%
%% Importing MFA to ets
%%
-spec import([atom()]) -> {ok, {tab(), tab()}} | error.
import(ModuleList) ->
	M2FA0 = ets:new(hint_m2fa, []),
	F2MA0 = ets:new(hint_f2ma, [bag]),
	lists:foldl(fun(Modname, {ok, {M2FA, F2MA}}) ->
				case Modname:module_info() of
					[{exports, FAList} | _T] ->
						import({Modname, FAList}, {M2FA, F2MA});
					_ ->
						error
				end
		end, {ok, {M2FA0, F2MA0}}, ModuleList).


%%
%% Internal
%%
-spec import({atom(), [{atom(), byte()}]}, {tab(), tab()}) -> tab().
import({M, [{F, A}|FAs]}, {M2FA, F2MA}) ->
	case F of
		module_info ->
			import({M, FAs}, {M2FA, F2MA});
		RelevantF ->
			?debugFmt("Adding function ~p/~p from module ~p to ets", [RelevantF, A, M]),
			ets:insert(M2FA, {RelevantF, A, M}),
			ets:insert(F2MA, {M, A, RelevantF}),
			import({M, FAs}, {M2FA, F2MA})
	end;

import({_M, []}, {M2FA, F2MA}) ->
	{ok, {M2FA, F2MA}}.
