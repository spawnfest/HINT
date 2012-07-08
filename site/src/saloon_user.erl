-module(saloon_user).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).

-include_lib("../../deps/saloon/include/user.hrl").

init({_Any, http}, Req, [Type]) ->
	saloon_init:prepare(Req),
	io:format("Initializing saloon_user with ~p~n", [Type]),
	{ok, Req, undefined}.

handle(Req, State) ->
	{ok, Rep} = case cowboy_http_req:method(Req) of
		{'GET', _} ->
			saloon_ctx:fill(),
			cowboy_http_req:reply(
				200, [], mustache:render(fril_register_view, "view/index.mustache"), Req
			);
		{'POST', _} ->
			case validate_registration(Req) of
				{ok, {Profile, EncPassword}} -> 
					saloon_ctx:fill(),
					cowboy_http_req:reply(
						302, 
						[
							{<<"Location">>, <<"/">>},	
							cowboy_cookies:cookie(
								<<"auth">>, 
								saloon_util:to_binary(saloon_auth:add_user(Profile, {md5, EncPassword})),
								[]
							)
						], <<"Redirecting...">>,
						Req
					);
				{error, Er} -> 
					saloon_ctx:errors(Er),
					saloon_ctx:fill(),
					cowboy_http_req:reply(
						200, [], mustache:render(fril_register_view, "view/index.mustache"), Req
					)
			end
	end,
	{ok, Rep, State}.

terminate(_R, _S) ->
	ok.


%%
%% Private funcitons.
%%

validate_registration(Req) ->
	BodyQs = element(1, cowboy_http_req:body_qs(Req)),
	Unfilled = [{X, <<"unfilled">>} || {X, Y} <- BodyQs, Y == <<"">>],
	Mismatch = case saloon_util:pk(<<"password">>, Req) == saloon_util:pk(<<"confirm">>, Req) of
		false -> [{<<"passwords">>, <<"dont_match">>}];
		_ -> []
	end,
	Unconfirmed = case saloon_util:pk(<<"agree">>, Req) of
		undefined -> [{<<"obey">>, <<"eula">>}];
		_ -> []
	end,
	NameTaken = [], %% TODO: FIXME - now it just doesn't register user and redirects to /
	EmailInvalid = [], %% TODO: FIXME - now isn't checked!
	case Unfilled ++ Mismatch ++ Unconfirmed ++ NameTaken ++ EmailInvalid of 
		[] -> {ok, {
					#profile{
						firstname=saloon_util:pk(<<"first_name">>, Req),
						lastname=saloon_util:pk(<<"last_name">>, Req),
						email=saloon_util:pk(<<"email">>, Req),
						roles=[user]
					}, saloon_util:md5(saloon_util:pk(<<"password">>, Req))
				}};
		Error -> {error, Error}
	end.
