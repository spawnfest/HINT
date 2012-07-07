%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 HINT Spawnfest Team
%%%-------------------------------------------------------------------

-module(hint_importer).
-export([import/1, import/2]).
-export([import_to_file/2, import_to_file/3]).

-type filename() :: file:filename().
-type idx_elem() :: 
	{{non_neg_integer(), 
			non_neg_integer(), 
			erl_types:erl_type()},
		mfa()}.
-type idx_term() :: [idx_elem()].

-spec import([filename()]) -> idx_term().
import(Files) -> 
	import(Files, []).

-spec import([filename()],[filename()]) -> idx_term().
import(Files, Includes) ->
	Specs = typer_guts:hack_start(Files, Includes),
	lists:concat([produce_idx_elems(S) || S <- Specs]).

import_to_file(Outfile, Files) ->
	import_to_file(Outfile, Files, []).

import_to_file(Outfile, Files, Includes) ->
	IT  = import(Files, Includes),
	ITB = term_to_binary(IT),
	file:write_file(Outfile, ITB).

-spec produce_idx_elems({mfa(), [erl_types:erl_type()]}) -> 
	idx_term().
produce_idx_elems({MFA, ArgsT}) ->
	produce_idx_elems(MFA, ArgsT, 0).

produce_idx_elems(_MFA, [], _Nth) -> [];
produce_idx_elems({_,_,Ar}=MFA, [AType|ArgsT], Nth) 
		when is_integer(Nth) ->
	[{{Ar, Nth, AType}, MFA} | produce_idx_elems(MFA, ArgsT, Nth+1)].

