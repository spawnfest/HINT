-module(saloon_main).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).

-include_lib("eunit/include/eunit.hrl").

init({_Any, http}, Req, []) ->
	?debugMsg("init"),
	{ok, Req, 0}.

handle(Req, State) ->
	?debugMsg("handle"),
	saloon_init:prepare(Req),
	?debugFmt(
		"erlydtl:compile: ~p~n", 
		[
			erlydtl:compile(
				"site/templates/base_landing.dtl", 
				main_dtl, 
				[{out_dir, "site/ebin/"}, {custom_tags_modules, [saloon_lang]}]
			)
		]
	),
	{ok, Rendered} = main_dtl:render([
		%{name, <<"Forever Alone Guy">>},
		%{friends, []},
		%{primes, ["2", <<"3">>, 5, 7]}
		{me, [
				{category, <<"">>},
				{controller, <<"saloon_main">>}
			]},
		{categories, []},
		{scripts, []}, 
		{stylesheets, [
			[{name, <<"landing">>}]
		]}
	]),
	?debugFmt("=RENDERING===~n~p~n", [Rendered]),
	{ok, Rep} = cowboy_http_req:reply(
		200, [], Rendered, Req
	),
	%{ok , Rep} = cowboy_http_req:reply(200, [], "ok", Req),
	{ok, Rep, State+1}.

terminate(_R, _S) ->
	ok.
