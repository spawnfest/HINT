%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 HINT Spawnfest Team
%%%-------------------------------------------------------------------

-module(hs_engine_types).

-export([q/1, generate_magic_file/2]).

temp_mod_name() -> 
	atom_to_list(?MODULE) ++ "_temp_mod".

temp_file() ->
	Ext = ".erl",
	Dir = mochitemp:mkdtemp(),
	Path = filename:join(Dir, [temp_mod_name(), Ext]),
	{ok, IO} = file:open(Path, [append]),
	{Path, IO}.

rm_temp_file(Path, IO) ->
	file:close(IO),
	io:format("~p", [Path]).
	%mochitemp:rmtempdir(filename:dirname(Path)).

q(Request) ->
	Req     = hint_search_req:new(Request),
	{Path, IO}   = temp_file(),
	{ok, M2F} = generate_magic_file(Req, IO),

	R = analyze_file(Path),
	rm_temp_file(Path, IO),

	OriginalModule = hint_search_req:module(Req),
	rank(M2F, OriginalModule, R).

-define(FIRST_FN, 1).

split_bod_inf({Func, {M,F,_A}=MFA, Bods}, Mag) ->
  	BodsLength = length(Bods),
	FNs  = [begin
				N = gen_magic_wrapper_func_name(Func, M, F, I) -- "''",
				B = iolist_to_binary(N),
				binary_to_atom(B, latin1)
			end || I <- lists:seq(?FIRST_FN,BodsLength)],
	Mag1 = lists:foldl(fun(FNi, PrevMag) -> 
					dict:store(FNi, {MFA, BodsLength}, PrevMag) 
			end, Mag, FNs),
	{Bods, Mag1}.

generate_magic_file(Req, IO) ->
	Header = gen_magic_header(),
	ok = file:write(IO, Header),

	Arity  = hint_search_req:arity(Req),
	Func   = hint_search_req:func(Req),
	String = hint_search_req:string(Req),

	AFuncs = funcs_with_arity(Arity),

	BodInf = 
		[{Func, MFA, gen_magic_wrappers({Func, String, Arity}, MFA)}
			|| MFA <- AFuncs],
	F = fun(El, Acc) ->
	        {Bodies, Acc1} =  split_bod_inf(El, Acc),
	        ok = file:write(IO, Bodies),
	        Acc1
	    end, 
	Magic2F = lists:foldl(F, dict:new(), BodInf),
	{ok, Magic2F}.

gen_magic_header() ->
	["-module('",temp_mod_name(),"').\n",
		"-compile(export_all).\n\n"].

funcs_with_arity(Ar) 
	when is_integer(Ar) ->
	plt_cache_server:lookup(Ar). 

-define(MAX_PERMS, 5).

perms([]) -> 
	[[]];
perms(L) when (length(L) =< ?MAX_PERMS) -> 
	[[H|T] || H <- L, T <- perms(L--[H])];
perms(L) -> 
	%TODO rotations
	[L].

gen_magic_wrappers({Func, Str, A}, {M,F,A}) ->
	Vars = [["V",to_s(I)] || I <- lists:seq(1,A)],
	Perm = perms(Vars),
	gen_magic_wrappers_(?FIRST_FN, Perm, Vars, 
		{to_s(Func), Str}, 
		{to_s(M), to_s(F), A}).

gen_magic_wrappers_(_N, [], _V, _FS, _MF) -> [];
gen_magic_wrappers_(I, [P|Perm], Vars, FS, MFA) ->
	Spec = gen_magic_wrapper_spec(I, FS, MFA),
	Body = gen_magic_wrapper_body(I, FS, MFA, P, Vars),
	[[Spec, $\n, Body, $\n] | 
		gen_magic_wrappers_(I+1, Perm, Vars, FS, MFA)].

gen_magic_wrapper_func_name(UFunc, M, F, I) ->
	[$', to_s(UFunc), "_m_", to_s(M), "_f_", to_s(F), $_, to_s(I), $'].

gen_magic_wrapper_spec(I, {Func, TStr}, {M, F, _A}) ->
	["-spec ",
		gen_magic_wrapper_func_name(Func, M, F, I),
		TStr, "."].

gen_magic_wrapper_body(I, {Func, _S}, {M, F, _A}, P, Vars) ->
	[gen_magic_wrapper_func_name(Func, M, F, I), 
		"(", gen_vars(Vars), ") ->\n",
		$\t, "'",M,"':'",F,"'(",gen_vars(P),").\n"].

gen_vars([V]) -> [V];
gen_vars([V | VL]) ->
	[[V,","], gen_vars(VL)].

to_s(A) when is_atom(A) ->
	atom_to_binary(A, latin1);
to_s(I) when is_integer(I) ->
	integer_to_list(I);
to_s(S) -> S.


analyze_file(FName) -> 
	plt_cache_server:apply({hs_dialyzer_wrapper, run, [FName]}).

% [{warn_contract_types,{"/var/folders/xf/qpds9v9x6pl49vwc4435hnz00000gp/T/WWKceZtmp/hs_engine_types_temp_mod.erl",
%                        4},
%                       {invalid_contract,[hs_engine_types_temp_mod,
%                                          fun_m_hipe_dot_f_translate_digraph_1,3,
%                                          "(digraph(),string(),string()) -> 'ok'"]}}

rank(PermDict, OriginalModule, DiLog) -> 
	RankDict = dict:new(),
	Result = match_log(PermDict, DiLog, RankDict),
	List = dict:to_list(
	         dict:map(fun(Key, Value) -> 
	           mfa_rank(OriginalModule, Key, Value)
	         end, Result)),
	lists:reverse(lists:keysort(2, List)).
	
match_log(_, [], RankDict) ->
	RankDict;
match_log(PermDict, [{_, _, {Warn, [_Mod, Perm | _]}} | T], RankDict) ->
	{ActualFun, PermCount} = dict:fetch(Perm, PermDict),
	RankDict1 = dict:append(ActualFun, {Perm, Warn, PermCount}, RankDict),
	match_log(PermDict, T, RankDict1).
	
mfa_rank(OriginalModule, {Mod, _F, _A}, Perms) ->
	RankMod = rank_module(OriginalModule, Mod),	
	RankFun = rank_fun(Mod),
	RankPerms = 
		case full_match(Perms) of
			false ->   %% one of perms successfully matched the request
				0.5;   %% and doesn't appear in dialyzer logs
			true  ->
				rank_perms(Perms)
		end,
	lists:sum([RankMod, RankFun, RankPerms, 1]).
	
rank_module(Original, Mod) when is_list(Original)->
	rank_module(binary_to_atom(iolist_to_binary(Original), latin1), Mod);
rank_module(Mod, Mod) ->
	0.5;
rank_module(_, Mod) ->
	Length    = length(atom_to_list(Mod)),
	if 
		Length > 2, Length < 7 -> 0;
		true -> 
			Deviation = math:pow(erlang:abs(10 - Length), 2),
			-1*Deviation*0.1
	end.

rank_fun(Fun) ->
	Length = length(atom_to_list(Fun)),
	if
		Length > 10 -> -1*0.005;
		true        -> 0
	end.

full_match([]) ->
	false;
full_match([{_, _, Length}|_] = Perms) ->
	length(Perms) =:= Length.

rank_perms(Perms) ->
	lists:max(lists:map(fun rank_warning/1, Perms)).

%% https://github.com/erlang/otp/blob/master/lib/dialyzer/src/dialyzer.erl
rank_warning({_Perm, contract_diff, _}) ->
	-0.5;
rank_warning({_Perm, contract_subtype, _}) ->
	-0.1;
rank_warning({_Perm, contract_supertype, _}) ->
	-0.09;
rank_warning({_Perm, contract_range, _}) ->
	-0.2;
rank_warning({_Perm, invalid_contract, _}) ->
	-0.7;
rank_warning({_Perm, extra_range, _}) ->
	-0.2;
% rank_warning({_Perm, overlapping_contract, _}) ->
% 	0;
% rank_warning({_Perm, spec_missing_fun, _}) ->
% 	0;
rank_warning(_) ->
	-0.9.
	