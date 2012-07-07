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
        , update/0
        , apply/1
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
-spec load(Path::string()) -> ok.
load(File) ->
  gen_server:call(?MODULE, {load, File}).

%%
%% Update all info from the currently loaded file
%%
-spec update() -> ok.
update() ->
  gen_server:call(?MODULE, update).

%%
%% Apply a specified function. Plt will be prepended to the list of arguments
%%
-spec apply({Module::atom(), Func::atom(), Args::list()}) -> any().
apply(MFA) ->
  gen_server:call(?MODULE, {apply, MFA}).


%%%
%%% Callbacks
%%%

init(Args) ->
  Plt = case proplists:lookup(file, Args) of
          undefined -> [];
          {_, File} -> dialyzer_plt:from_file(File)
        end,
  Ets = create_and_update_ets(Plt),
  {ok, #state{plt = Plt, ets = Ets}}.

handle_call({load, File}, _From, #state{ets=Ets0}) ->
  Plt  = dialyzer_plt:from_file(File),
  Ets1 = ets:delete_all_objects(Ets0),
  Ets  = update_ets(Ets1, Plt),
  {reply, ok, #state{ets=Ets, plt=Plt, file=File}};

handle_call(update, _From, #state{ets=Ets0, file=File}) ->
  Plt  = dialyzer_plt:from_file(File),
  Ets1 = ets:delete_all_objects(Ets0),
  Ets  = update_ets(Ets1, Plt),
  {reply, ok, #state{ets=Ets, plt=Plt}};

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
  
create_and_update_ets(Plt) ->
  Ets = ets:new(?MODULE, [ ordered_set
                         , public
                         , {write_concurrency,true}
                         , {read_concurrency,true}]),
  update_ets(Ets, Plt).

update_ets(Ets0, Plt) ->
  Modules = sets:to_list(dialyzer_plt:all_modules(Plt)),
  MFAList = get_mfas(Modules, Plt),
  Ets = ets:insert(Ets0, MFAList),
  Ets.

get_mfas(Modules, Plt) -> get_mfas(Modules, Plt, []).

get_mfas([], _, Acc)     ->
  Acc;
get_mfas([H|T], Plt, Acc0) ->
  {value, MFATypes} = dialyzer_plt:lookup_module(Plt,H),
  Acc1 = mfa_from_types(MFATypes, Acc0),
  get_mfas(T, Plt, Acc1).

mfa_from_types([], Acc)                -> Acc;
mfa_from_types([{MFA, _, _} | T], Acc) -> mfa_from_types(T, [MFA | Acc]).

