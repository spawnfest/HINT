-module(saloon_search).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).

-include_lib("eunit/include/eunit.hrl").

init(_, Req, _) ->
	{ok, Req, 0}.

handle(Req, State) ->
	?debugMsg("handle"),
	case cowboy_http_req:method(Req) of
		{'POST', _} ->
			R = case cowboy_http_req:body_qs(Req) of
				   {[{<<"search">>, QB}], _} ->
						Q = binary_to_list(QB),
						{ok, Data} = hint_search:q(Q),
						Data1 = lists:sublist(Data, 10),
						render(Data1);
					_ -> 
					io:format("REQ ~p~n", [ooooo]),
					render([])
				end,
			{ok, Rep} = cowboy_http_req:reply(
				200, [], R, Req
			),
			{ok, Rep, State};
		{'GET', _} ->
			cowboy_http_req:reply(
				301, [], "Stop!", Req
			)
	end.

terminate(_R, _S) ->
	ok.

render(Data) ->
	D = [to_href(M) || M <- Data],
	erlydtl:compile(
		"site/priv/templates/search.dtl", 
		hint_search, 
		[{out_dir, "site/ebin/"}, {custom_tags_modules, [saloon_lang]}]
	),

	{ok, Rendered} = hint_search:render([
		{data, D}
	]),
	Rendered.

to_href({MFA, Weight}) ->
	FunctionName = function_name(MFA),
	W = io_lib:format("~.1f",[Weight]),
	lists:append([ "<a href=\"http://erlanger.org/man/", function_name_link(MFA), "\">"
	             , FunctionName, "</a><small>", W ,"</small><br />"]).

function_name({M, F, A}) ->
	lists:flatten(lists:append([atom_to_list(M)
	             , "#", atom_to_list(F), "/", integer_to_list(A)])).
function_name_link({M, F, A}) ->
	lists:flatten(lists:append([atom_to_list(M)
	             , ".html#", atom_to_list(F), "/", integer_to_list(A)])).
