-module(saloon_conf).

-export([
		dispatch/0,
		port/0,
		languages/0
	]).


dispatch() ->
	[
		{'_', [
				%%
				%% Site controllers
				%%

				{[<<"search">>, '...'], saloon_search, ["search"]},
				{[<<"register">>, '...'], saloon_user, ["register"]},
				{[<<"login">>, '...'], saloon_user, ["login"]},
				{[<<"logout">>, '...'], saloon_user, ["logout"]},

				%%
				%% Default handlers
				%%

				{[<<"static">>, '...'], cowboy_http_static, 
					[
						{directory, <<"./static">>},
						{mimetypes, [
								{<<".txt">>, [<<"text/plain">>]},
								{<<".html">>, [<<"text/html">>]},
								{<<".htm">>, [<<"text/html">>]},
								{<<".css">>, [<<"text/css">>]},
								{<<".js">>, [<<"application/javascript">>]}
						]}
					]
				},
				{'_', saloon_main, []}
			]}
	].

languages() -> [en, ru, lv, es].

port() -> 52012.
