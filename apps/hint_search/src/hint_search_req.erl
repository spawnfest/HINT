%% Consider 
%%   mod:func([A],{A,B},list(), any(), some_type({A,list(),B})) -> ok | error.
%%
%% Start from the beginning. Scan each character
%%
%% 1. Once we hit ":", we've got our module name, start looking for function
%%    name.
%% 2. Once we hit "(", we've got our method name, start looking for arguments
%% 3. Arguments are a bit trickier
%%    3.1 If we encounter a "{", "[", "(" we don't care about anything 
%%        until we hit a matching closing paranthesis/bracket. Every time
%%        we hit an opening paranthesis/bracket, we increase depth level.
%%        Once we hit a closing p/b, we decrease depth level
%%        We only care about arguments on level 0
%%    3.2 Arguments are spearated by commas. If we hit a comma on depth 0,
%%        we increment the counter
%%    3.3 Once we hit a ")" on depth 0
%%        3.3.1 If there were no characters between thhe opening paranthesis and 
%%              this one, we return 0
%%              Example: mod:func()
%%        3.3.2 If there were characters, we increase our counter by one and return
%%              it
%%              Example: mod:func(a)    %% 0 commas, 1 argument
%%              Example: mod:func(a, b) %% 1 comma,  2 arguments
%% 4. Discard the rest of the string, return {Mod, Func, Arity}
%%

-module(hint_search_req).

%%% Exports
-export([parse/1, test/0]).
-export([new/1, module/1, func/1, arity/1, string/1]).

%%% Defines

-record(req, { mod
             , func
             , arity = 0
             , string = ""
             }).

%%% API

parse(String) ->
  parse_module(lists:flatten(String), []).

new(Request) ->
	{M, F, A, S} = parse(Request),
	#req { mod=M
	     , func=F
	     , arity=A
	     , string=S
	     }.

module(Req) -> Req#req.mod.
func(Req)   -> Req#req.func.
arity(Req)  -> Req#req.arity.
string(Req) -> Req#req.string.

%%% Internal

%% Bullet point 1.
parse_module([$:|T], Acc)        ->
  Mod = list_to_atom(lists:reverse(Acc)),
  {Func, Arity, Rest} = parse_func(T),
  {Mod, Func, Arity, Rest};
parse_module([$(|T] = Rest, Acc) ->
  {Func, Arity, Rest} = parse_func(Rest, Acc),
	{[], Func, Arity, Rest};
parse_module([H|T], Acc)  ->
  parse_module(T, [H | Acc]).

%% Bullet point 2.
parse_func(L) ->
  parse_func(L, []).

parse_func([$(|T] = Rest, [])  ->
  {[], parse_arity(T), Rest};
parse_func([$(|T] = Rest, Acc) ->
  Func = list_to_atom(lists:reverse(Acc)),
  {Func, parse_arity(T), Rest};
parse_func([H|T], Acc)  ->
  parse_func(T, [H | Acc]).

%% Bullet point 3.
parse_arity(L) ->
  parse_arity(L, 0, 0, 0).

parse_arity([$)|_], Acc, 0=_Chars, 0=_Depth) -> Acc;
parse_arity([$)|_], Acc, _Chars, 0=_Depth)   -> Acc + 1;
parse_arity([$,|T], Acc, Chars, 0 = Depth)   ->
  parse_arity(T, Acc + 1, Chars + 1, Depth);
parse_arity([Char|T], Acc, Chars, Depth)     ->
  parse_arity(T, Acc, Chars+1, maybe_change_depth(Depth, Char)).

maybe_change_depth(Depth, Char) when        Char =:= $)
                                     orelse Char =:= $]
                                     orelse Char =:= $} ->
  Depth - 1;
maybe_change_depth(Depth, Char) when        Char =:= $(
                                     orelse Char =:= $[
                                     orelse Char =:= ${ ->
  Depth + 1;
maybe_change_depth(Depth, _)                            ->
  Depth.


%%=============================================================================

test() ->
  {mod, func, 0, _} = parse("mod:func()"),
  {mod, func, 1, _} = parse("mod:func(a)"),
  {mod, func, 5, _} = 
    parse([ "mod:func([A],{A,B},list(), any(), some_type({A,list(),B})) -> "
          , "ok | error."]),
  {mod, func, 2, _} = parse("mod:func(A, type()) when A::list() -> ok | error."),
  {[],  func, 0, _} = parse("func()"),
  {[],  func, 1, _} = parse("func(a)"),
  {[],  func, 2, _} = parse("func(A, type()) when A::list() -> ok | error."),
  {mod, [],   0, _} = parse("mod:()"),
  {mod, [],   1, _} = parse("mod:(a)"),
  {mod, [],   2, _} = parse("mod:(A, type()) when A::list() -> ok | error."),
  ok.