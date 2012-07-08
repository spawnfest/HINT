-module(hint_query_worker).
-behaviour(gen_server).

-export([q/1]).

%%
%% gen_server callbacks
%%
-export([ start_link/0
        , init/1
        , code_change/3
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        ]).

q(Request) ->
	{ok, Pid} = supervisor:start_child(hint_query_sup, []),
	Result = gen_server:call(Pid, {q, Request}, infinity),
	supervisor:terminate_child(hint_query_sup, Pid),
	Result.

start_link() ->
	gen_server:start_link(?MODULE, [], []).

%%
%% gen_server callbacks
%%

init(_Args) ->
    {ok, []}.

handle_call({q, Req}, _From, State) ->
	R = try hs_engine_types:q(Req) of
			L -> {ok, L}
		catch 
			_:Reason -> {error, Reason}
		end,
    {reply, R, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(shutdown, _State) ->
    ok.
