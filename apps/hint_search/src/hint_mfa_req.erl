%%
%% "m:f/a" -> 
%% [ {exact, {ModulePattern, FunctionPattern, Arity}}
%% , {starts_with, {ModulePattern1, FunctionPattern1, Arity1}
%% , {contains, {ModulePattern2, FunctionPattern2, Arity2}}]
%%
-module(hint_mfa_req).

%%
%% Exports
%%
-export([ new/1
		, module/1
		, func/1
		, arity/1
		, fuzzy_matches/1
	]).

%%
%% Defines
%%
-record(mfa, {m,f,a}).

-spec new(string()) -> {mfa, atom(), atom(), byte()}.
new(Req) ->
	{M, F, A} = parse(Req),
	#mfa{m=M, f=F, a=A}.

module(Mfa) ->
	[ {exact, Mfa#mfa.m}
	, {starts_with, Mfa#mfa.m ++ "(.*)"}
	, {contains, glue_letters(Mfa#mfa.m, "(.*)")}
	].

func(Mfa) ->
	[ {exact, Mfa#mfa.f}
	, {starts_with, Mfa#mfa.f ++ "(.*)"}
	, {contains, glue_letters(Mfa#mfa.f, "(.*)")}
	].

arity(Mfa) ->
	Mfa#mfa.a.

fuzzy_matches(Match) ->
	[ {exact, Match}
	, {starts_with, Match ++ "(.*)"}
	, {contains, glue_letters(Match, "(.*)")}
].

%%
%% Internal
%%

-spec glue_letters(string(), string()) -> string().
glue_letters(String, GlueString) ->
	case String of
		[] -> "";
		_  -> 
			lists:foldl(fun (Char, Acc) -> 
						Acc ++ [Char] ++ GlueString 
				end, GlueString, String)
	end.


%% parse
parse(String) ->
	parse_module(lists:flatten(String), []).

%% Bullet point 1.
parse_module([$:|T], Acc)        ->  
	Mod = lists:reverse(Acc),
	{Func, Arity} = parse_func(T),
	{Mod, Func, Arity};
parse_module([$/|_T] = Rest, Acc) ->
	{Func, Arity} = parse_func(Rest, Acc),
	{[], Func, Arity};
parse_module([H|T], Acc)  ->  
	parse_module(T, [H | Acc]);
parse_module([], Acc) ->
	FnM = lists:reverse(Acc),
	{FnM, FnM, []}.

%% Bullet point 2.
parse_func(L) ->
	parse_func(L, []).

parse_func([$/|T], [])  ->  
	{[], parse_arity(T)};
parse_func([$/|T], Acc) ->
	Func = lists:reverse(Acc),
	{Func, parse_arity(T)};
parse_func([H|T], Acc)  ->  
	parse_func(T, [H | Acc]);
parse_func([], Acc) ->
	{lists:reverse(Acc), []}.

%% Bullet point 3.
parse_arity(L) ->
	list_to_integer(L).
