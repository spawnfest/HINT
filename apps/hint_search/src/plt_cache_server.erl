-module(plt_cache_server).
-behaviour(gen_server).

%%%
%%% Exports
%%%

%%
%% API
%%

-export([ start_link/1
        , load/1
        , load/2
        , update/0
        , update/1
        , apply/1
        , apply/2
        , lookup/1
        , lookup/2
        ]).

%%
%% gen_server callbacks
%%
-export([ init/1
        , code_change/3
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        ]).

%%%
%%% Defines
%%%

-record(state, {plt, ets, file}).

%%%
%%% API
%%%

-spec start_link([{file, Path::string()}] | []) -> ok.
start_link(Args) ->
  Name = proplists:get_value(name, Args, ?MODULE),
  gen_server:start_link({local, Name}, ?MODULE, Args, []).

%%
%% Load a specified Plt file. 
%%
-spec load(pid(), Path::string()) -> ok.
load(Server, File) ->
  gen_server:call(Server, {load, File}).
load(File) ->
  gen_server:call(?MODULE, {load, File}).

%%
%% Update all info from the currently loaded file
%%
-spec update(pid()) -> ok.
update(Server) ->
  gen_server:call(Server, update).
update() ->
  gen_server:call(?MODULE, update).

%%
%% Apply a specified function. Plt will be prepended to the list of arguments
%%
-spec apply(pid(), {Module::atom(), Func::atom(), Args::list()}) -> any().
apply(Server, MFA) ->
  gen_server:call(Server, {apply, MFA}, infinity).
apply(MFA) ->
  ?MODULE:apply(?MODULE, MFA).

%%
%% Lookup arity
%%
-spec lookup(pid(), integer()) -> any().
lookup(Server, Arity) ->
  gen_server:call(Server, {lookup, Arity}).
lookup(Arity) ->
  gen_server:call(?MODULE, {lookup, Arity}).

%%%
%%% Callbacks
%%%

init(Args) ->
  File = get_file(Args),
  Plt = dialyzer_plt:from_file(File),
  Ets = create_and_update_ets(Plt),
  {ok, #state{plt = Plt, ets = Ets, file = File}}.

handle_call({load, File}, _From, #state{ets=Ets} = State) ->
  Plt  = dialyzer_plt:from_file(File),
  clean_update(Ets, Plt),
  {reply, ok, State#state{plt=Plt, file=File}, hibernate};

handle_call(update, _From, #state{ets=Ets, file=File} = State) ->
  Plt  = dialyzer_plt:from_file(File),
  clean_update(Ets, Plt),
  {reply, ok, State, hibernate};

handle_call({lookup, Arity}, _From, #state{ets=Ets} = State) ->
  {reply, ets:lookup(Ets, Arity), State};

handle_call({apply, {M, F, A}}, _From, #state{plt=Plt} = State) ->
  {reply, erlang:apply(M, F, [Plt | A]), State, hibernate}.

handle_cast(_, State) ->
  {noreply, State}.

handle_info(_, State) ->
  {noreply, State}.

code_change(_, State, _) ->
  {ok, State}.

terminate(shutdown, #state{ets=Ets}) ->
  ets:delete(Ets),
  ok.

%%%
%%% Internal
%%%

get_file(Args) ->
  File = proplists:get_value(file, Args, undefined),
  case File of
    undefined -> 
      case application:get_env(plt_path) of
        undefined              -> dialyzer_plt:get_default_plt();
        {ok, {priv_dir, PrivFile}} ->
            {ok, AppName} = application:get_application(),
            PrivDir = filename:absname(code:priv_dir(AppName)),
            filename:absname_join(PrivDir, PrivFile);
        {ok, PltPath}    -> filename:absname(PltPath)
      end;
    Path      -> filename:absname(Path)
  end.

create_and_update_ets(Plt) ->
  Ets = ets:new(?MODULE, [ bag
                         , private
                         , {keypos, 3}
                         , {write_concurrency,true}
                         , {read_concurrency,true}]),
  update_ets(Ets, Plt),
  Ets.

clean_update(Ets, Plt) ->
  ets:delete_all_objects(Ets),
  update_ets(Ets, Plt).

update_ets(Ets0, Plt) ->
  AllModules = sets:to_list(dialyzer_plt:all_modules(Plt)),
	% wrong heuristic :)
  Modules = [M || M <- AllModules,
			string:equal(atom_to_list(M),
				string:to_lower(atom_to_list(M)))],
  MFAList = get_mfas(Modules, Plt),
  ets:insert(Ets0, MFAList).

get_mfas(Modules, Plt) -> get_mfas(Modules, Plt, []).

get_mfas([], _, Acc)     ->
  Acc;
get_mfas([H|T], Plt, Acc0) ->
  {value, MFATypes} = dialyzer_plt:lookup_module(Plt,H),
  Acc1 = mfa_from_types(MFATypes, Acc0),
  get_mfas(T, Plt, Acc1).

mfa_from_types([], Acc)                -> Acc;
mfa_from_types([{MFA, _, _} | T], Acc) -> mfa_from_types(T, [MFA | Acc]).

