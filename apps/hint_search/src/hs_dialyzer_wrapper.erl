%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 HINT Spawnfest Team
%%%-------------------------------------------------------------------

-module(hs_dialyzer_wrapper).
-export([run/3, run/2, run/1, go_native/0]).

-include_lib("dialyzer/src/dialyzer.hrl").

dopts(File, Inc) ->
	[
		{files, [File]}, 
		{include_dirs, Inc},
		{warnings, 
			[overspecs,
				underspecs,
				specdiffs,
				no_return,
				no_unused,
				no_improper_lists,
				no_fun_app,
				no_match,
				no_opaque]},
		{from, src_code}]. 


run(File) ->
	dialyzer:run(dopts(File, [])).

run(PLT, File) ->
	run(PLT, File, []).

run(PLT, File, Includes) ->
	try dialyzer_options:build([
				{report_mode, quiet},
				{erlang_mode, true}
				|dopts(File,Includes)]) of
		{error, Msg} ->
			throw({dialyzer_error, Msg});
		OR ->
			OR1 = OR#options{ init_plts={prebuilt,PLT} },
			case dialyzer_cl_hint:start(OR1) of
				{?RET_DISCREPANCIES, Warnings} -> Warnings;
				{?RET_NOTHING_SUSPICIOUS, []}  -> []
			end
	catch
		throw:{dialyzer_error, ErrorMsg} ->
			erlang:error({dialyzer_error, lists:flatten(ErrorMsg)})
	end.

go_native() ->
	case erlang:system_info(hipe_architecture) of
		undefined -> ok;
		_ ->
			Mods = [lists, dict, gb_sets, gb_trees, ordsets, sets,
					cerl, cerl_trees, erl_types, erl_bif_types,
					dialyzer_analysis_callgraph, dialyzer_codeserver,
					dialyzer_dataflow, dialyzer_dep, dialyzer_plt,
					dialyzer_succ_typings, dialyzer_typesig],
			dialyzer_cl_hint:native_compile(Mods)
	end.
