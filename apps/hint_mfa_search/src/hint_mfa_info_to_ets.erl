%%
%% Module for dev-mode only, will later be replaced with plt-based data feed
%%
-module(hint_mfa_info_to_ets).

-export([
		import/1,
		import/3
	]).

-include_lib("eunit/include/eunit.hrl").

-type tab() :: ets:tab().
%-type match_spec() :: ets:match_spec().

%%
%% Importing MFA to ets
%%
-spec import([atom()]) -> {ok, {tab(), tab()}} | error.
import(ModuleList) ->
	M2FA = ets:new(hint_m2fa, []),
	F2MA = ets:new(hint_f2ma, [bag]),
	import(ModuleList, M2FA, F2MA).

-spec import([atom()], tab(), tab()) -> {ok, {tab(), tab()}} | error.
import(ModuleList, M2FA0, F2MA0) ->
	lists:foldl(fun(ModuleName, {ok, FSetAcc0, {M2FA, F2MA}}) ->
				case code:ensure_loaded(ModuleName) of
					{module, _} ->
						[{exports, FAList} | _T] = ModuleName:module_info(),
						{ok, FSetAcc, _} = do_import({ModuleName, FAList}, {M2FA, F2MA}),
						{ok, sets:union(FSetAcc0, FSetAcc), {M2FA, F2MA}};
					{error, _} ->
						{ok, [], {M2FA, F2MA}}
				end
		end, {ok, sets:new(), {M2FA0, F2MA0}}, ModuleList).


%%
%% Internal
%%
-spec do_import({atom(), [{atom(), byte()}] | []}, {tab(), tab()}) -> {ok, tab(), tab()}.
do_import({M, FAs}, {M2FA, F2MA}) ->
	do_import({M, FAs}, sets:new(), {M2FA, F2MA}).

do_import({M, [{F, A}|FAs]}, FSetAcc, {M2FA, F2MA}) ->
	case F of
		module_info ->
			do_import({M, FAs}, FSetAcc, {M2FA, F2MA});
		_ ->
			?debugFmt("Adding function ~p/~p from module ~p to ets", [F, A, M]),
			ets:insert(M2FA, {F, A, M}),
			ets:insert(F2MA, {M, A, F}),
			do_import({M, FAs}, sets:add_element(atom_to_binary(F, utf8), FSetAcc), {M2FA, F2MA})
	end;

do_import({_M, []}, FSetAcc, {M2FA, F2MA}) ->
	{ok, FSetAcc, {M2FA, F2MA}}.
