%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 HINT Spawnfest Team
%%%-------------------------------------------------------------------

-module(hs_engine_types).

-compile(export_all).

temp_mod_name() -> 
	atom_to_list(?MODULE) ++ "_temp_mod".

req_type() -> req.

temp_file() ->
	Ext = ".erl",
	Dir = mochitemp:mkdtemp(),
	filename:join(Dir, [temp_mod_name(), Ext]).

rm_temp_file(FName) ->
	mochitemp:rmtempdir(filename:dirname(FName)).

% req is {{M,F,A}, "mod:fun(lits(),[foo(),bar()])"} ADT, where MFA is derived from string
q(Request) ->
	Req     = hint_search_req:new(Request),
	{ok, M2F, Data} = generate_magic_file(Req),
	FName   = temp_file(),
	ok      = file:write_file(FName, Data),
	{ok, R} = analyze_file(FName),
	rm_temp_file(FName),
	rank(M2F, R).

generate_magic_file(Req) ->
	Arity  = hint_search_req:arity(Req),
	AFuncs = funcs_with_arity(Arity),
	Func   = hint_search_req:func(Req),
	String = hint_search_req:string(Req),
	Header = gen_magic_header(),
	Bodies = 
		[gen_magic_wrappers({Func, String, Arity}, MFA) 
			|| MFA <- AFuncs],
	Magic2F = dict:new(), %% TODO
	{ok, Magic2F, [Header, Bodies]}.

gen_magic_header() ->
	["-module('",temp_mod_name(),"').\n",
		"-compile(export_all)\n\n"].

funcs_with_arity(A) 
	when is_integer(A) ->
	%% TODO
	[{lists,any,2},{foo,abr,2}].

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
	gen_magic_wrappers_(1, Perm, Vars, 
		{to_s(Func), Str}, 
		{to_s(M), to_s(F), A}).

gen_magic_wrappers_(_N, [], _V, _FS, _MF) -> [];
gen_magic_wrappers_(I, [P|Perm], Vars, FS, MFA) ->
	Spec = gen_magic_wrapper_spec(I, FS, MFA),
	Body = gen_magic_wrapper_body(I, FS, MFA, P, Vars),
	[[Spec, $\n, Body, $\n] | 
		gen_magic_wrappers_(I+1, Perm, Vars, FS, MFA)].

gen_magic_wrapper_func_name(UFunc, F, I) ->
	[UFunc, $_, F, $_, to_s(I)].

gen_magic_wrapper_spec(I, {Func, TStr}, {_M, F, _A}) ->
	["-spec ",
		gen_magic_wrapper_func_name(Func, F, I),
		TStr, "."].

gen_magic_wrapper_body(I, {Func, _S}, {M, F, _A}, P, Vars) ->
	[gen_magic_wrapper_func_name(Func, F, I), 
		"(", gen_vars(Vars), ") ->\n",
		$\t,M,$:,F,$(,gen_vars(P),$),$.].

gen_vars([V]) -> [V];
gen_vars([V | VL]) ->
	[[V,","], gen_vars(VL)].

to_s(A) when is_atom(A) ->
	atom_to_binary(A, latin1);
to_s(I) when is_integer(I) ->
	integer_to_list(I);
to_s(S) -> S.


analyze_file(FName) -> {ok, []}.

rank(_, _) -> 
	%TODO
	[].


