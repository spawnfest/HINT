-module(hs_engine_mfa).

%%%
%%% Exports
%%%

%%
%% API
%%
-export([
		mfa_to_ets/2
	]).

%%%
%%% Import
%%%

-include_lib("eunit/include/eunit.hrl").


%%
%% External
%%

mfa_to_ets(MFAList, {M2FA, F2MA}) -> 
	{Ms, Fs} = lists:foldl(fun({M, F, A}, {Mods, Funs}) ->
				ets:insert(M2FA, {F, A, M}),
				ets:insert(F2MA, {M, A, F}),
				Mods1 = sets:add_element(atom_to_binary(M, utf8), Mods),
				Funs1 = sets:add_element(atom_to_binary(F, utf8), Funs),
				{Mods1, Funs1}
		end, {sets:new(), sets:new()}, MFAList),
	?debugFmt("Ms ~p Fs ~p", [sets:to_list(Ms), sets:to_list(Fs)]),
	{Ms, Fs}.
