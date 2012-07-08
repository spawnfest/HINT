-module(plt_cache_server).
-behaviour(gen_server).

%%%
%%% Exports
%%%

%%
%% API
%%

-export([ start_link/1
        , load/2
        , update/1
        , apply/2
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

%%
%% Update all info from the currently loaded file
%%
-spec update(pid()) -> ok.
update(Server) ->
  gen_server:call(Server, update).

%%
%% Apply a specified function. Plt will be prepended to the list of arguments
%%
-spec apply(pid(), {Module::atom(), Func::atom(), Args::list()}) -> any().
apply(Server, MFA) ->
  gen_server:call(Server, {apply, MFA}).


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
  {reply, ok, State#state{plt=Plt, file=File}};

handle_call(update, _From, #state{ets=Ets, file=File} = State) ->
  Plt  = dialyzer_plt:from_file(File),
  clean_update(Ets, Plt),
  {reply, ok, State};

handle_call({apply, {M, F, A}}, _From, #state{plt=Plt} = State) ->
  {reply, erlang:apply(M, F, [Plt | A]), State}.

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
        undefined     -> dialyzer_plt:get_default_plt();
        {ok, PltPath} -> filename:absname(PltPath)
      end;
    Path      -> filename:absname(Path)
  end.

create_and_update_ets(Plt) ->
  Ets = ets:new(?MODULE, [ ordered_set
                         , public
                         , {write_concurrency,true}
                         , {read_concurrency,true}]),
  update_ets(Ets, Plt),
  Ets.

clean_update(Ets, Plt) ->
  ets:delete_all_objects(Ets),
  update_ets(Ets, Plt).

update_ets(Ets0, Plt) ->
  Modules = sets:to_list(dialyzer_plt:all_modules(Plt)),
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

