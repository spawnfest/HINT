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

modules_matching(Module, MFAs) ->
	?debugFmt(">MODULE>>> ~p", [Module]),
	case Module of
		[] -> MFAs;
		_   ->
			ModuleString = atom_to_list(Module),
			ModuleUniversum = atoms_to_utf_set([element(1, X) || X <- MFAs]),                     % I don't do drugs, honestly
			%?debugFmt(">RELEVANT MODULES>>>~n~p", [sets:to_list(ModuleUniversum)]),             %  it's just time for spawnfest running out
			MM = do_modules_matching(ModuleUniversum, hint_mfa_req:fuzzy_matches(ModuleString)), % ~jonn mostovoy
			?debugFmt("~n!!!!!!!????? MFA ~p", [[MFA || {M,_,_}=MFA <- MFAs, lists:member(M, MM)]]),
			[MFA || {M,_,_}=MFA <- MFAs, lists:member(M, MM)]
	end
.

%%%
%%% Internal
%%%

do_modules_matching(RelevantModules, ModExprs) ->
	_ModExact = sets:filter(fun(Element) -> %% ModExact, your hour will come.
				list_to_binary(proplists:get_value(exact, ModExprs)) == Element
		end, RelevantModules),
	[_ModStartsWith, ModContains] = [sets:filter(fun(Element) ->
				case re:run(Element, proplists:get_value(ReMode, ModExprs), []) of
					nomatch -> false;
					_ -> true
				end
		end, RelevantModules) || ReMode <- [starts_with, contains]],
	?debugMsg("Still Alive"),
	% Треш, угар, содомия
	[binary_to_atom(B,latin1)||B<-sets:to_list(ModContains)].
	%{sets:to_list(ModExact), sets:to_list(ModStartsWith), sets:to_list(ModContains)}.


-spec atoms_to_utf_set([atom()]) -> set().
atoms_to_utf_set(Atoms) ->
	sets:from_list([atom_to_binary(Atom, utf8) || Atom <- Atoms]).
