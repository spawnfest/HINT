-module(hint_mfa_search_server).
-behaviour(gen_server).

%%%
%%% Exports
%%%

%%
%% API
%%
-export([ start_link/1
		, store/1
		, search/1
	]).

%%
%% gen_server
%%
-export([ init/1
		, code_change/3
		, handle_call/3
		, handle_cast/2
		, handle_info/2
		, terminate/2
	]).

%%
%% Defines
%%

-record(state, {functions, modules, ets_m2fa, ets_f2ma}).
-define(SERVER, ?MODULE).

%%
%% Includes
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% API
%%

-spec start_link([atom()] | []) -> ok.
start_link(Args) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%%
%% Store data of module or several modules in M2FA (Module -> Function, Arity) set and
%% F2MA (Function -> Module, Arity) bag.
%%
-spec store(atom() | [atom()]) -> ok.
store(Module) when is_atom(Module) ->
	gen_server:call(?SERVER, {store, [Module]});
store(Modules) when is_list(Modules) ->
	gen_server:call(?SERVER, {store, Modules}).
	

%%
%% Perform search for the given search string.
%% TODO: fuzzy search
%%
-spec search(binary()) -> [mfa()].
search(SearchString) ->
	gen_server:call(?SERVER, {search, SearchString}).

%%
%% Callbacks
%%

init(Args) ->
	{ok, FSet, {M2FA, F2MA}} = hint_mfa_info_to_ets:import(Args),
	{ok, #state{functions=FSet, modules=atoms_to_utf_set(Args), ets_m2fa=M2FA, ets_f2ma=F2MA}}.

handle_call({store, Modules}, _From, State) when is_list(Modules) ->
	?debugFmt("Handling call having ~nModules: ~p~n Functions: ~p~n In State", [sets:to_list(State#state.modules), sets:to_list(State#state.functions)]),
	{ok, FSet, _} = hint_mfa_info_to_ets:import(Modules, State#state.ets_m2fa, State#state.ets_f2ma),
	case sets:size(FSet) of
		0 ->
			{reply, ok, State};
		_ ->
			MSet = atoms_to_utf_set(Modules),
			{reply, ok, State#state{functions=sets:union(FSet, State#state.functions), modules=sets:union(MSet, State#state.modules)}}
	end;
handle_call(_X,_Y,Z) ->
	{reply, error, Z}.
handle_cast(_, State) ->
	{noreply, State}.

handle_info(_, State) ->
	{noreply, State}.

code_change(_, State, _) ->
	{ok, State}.

terminate(shutdown, #state{ets_m2fa=M2FA, ets_f2ma=F2MA}) ->
	ets:delete(M2FA),
	ets:delete(F2MA),
	ok.

%%
%% Internal
%%

-spec atoms_to_utf_set([atom()]) -> set().
atoms_to_utf_set(Atoms) ->
	sets:from_list([atom_to_binary(Atom, utf8) || Atom <- Atoms]).
