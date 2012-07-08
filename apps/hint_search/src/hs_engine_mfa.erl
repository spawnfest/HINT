-module(hs_engine_mfa).

%%%
%%% Exports
%%%

%%
%% API
%%
-export([ mfa_to_ets/2
		, modules_matching/2
	]).

%%%
%%% Import
%%%

-include_lib("eunit/include/eunit.hrl").


%%%
%%% External
%%%

mfa_to_ets(MFAList, {M2FA, F2MA}) -> 
	{Ms, Fs} = lists:foldl(fun({M, F, A}, {Mods, Funs}) ->
				ets:insert(M2FA, {F, A, M}),
				ets:insert(F2MA, {M, A, F}),
				Mods1 = sets:add_element(atom_to_binary(M, utf8), Mods),
				Funs1 = sets:add_element(atom_to_binary(F, utf8), Funs),
				{Mods1, Funs1}
		end, {sets:new(), sets:new()}, MFAList),
	{Ms, Fs}.

modules_matching(Module, MFA) ->
	?debugFmt(">MODULE>>> ~p", [Module]),
	case Module of
		[] -> MFA;
		_   ->
			ModuleString = atom_to_list(Module),
			ModuleUniversum = atoms_to_utf_set([element(1, X) || X <- MFA]),               % I don't do drugs, it's just time for 
			%?debugFmt(">RELEVANT MODULES>>>~n~p", [sets:to_list(ModuleUniversum)]),       % spawnfest running out
			do_modules_matching(ModuleUniversum, hint_mfa_req:fuzzy_matches(ModuleString)) % ~jonn mostovoy
	end
.

%%%
%%% Internal
%%%

do_modules_matching(RelevantModules, ModExprs) ->
	ModExact = sets:filter(fun(Element) ->
				list_to_binary(proplists:get_value(exact, ModExprs)) == Element
		end, RelevantModules),
	[ModStartsWith, ModContains] = [sets:filter(fun(Element) ->
				case re:run(Element, proplists:get_value(ReMode, ModExprs), []) of
					nomatch -> false;
					_ -> true
				end
		end, RelevantModules) || ReMode <- [starts_with, contains]],
	{sets:to_list(ModExact), sets:to_list(ModStartsWith), sets:to_list(ModContains)}.


-spec atoms_to_utf_set([atom()]) -> set().
atoms_to_utf_set(Atoms) ->
	sets:from_list([atom_to_binary(Atom, utf8) || Atom <- Atoms]).
