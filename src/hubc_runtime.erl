-module(hubc_runtime).

-behavior(e2_service).

-export([start_link/0, for_section/2, init_train_op/3,
         init_eval_op/4]).

-export([init/1, handle_msg/3]).

-record(state, {cache}).

%% ===================================================================
%% Start / init
%% ===================================================================

start_link() ->
    e2_service:start_link(?MODULE, [], [registered]).

init([]) ->
    {ok, #state{cache=dict:new()}}.

%% ===================================================================
%% API
%% ===================================================================

for_section(Section, Project) ->
    e2_service:call(?MODULE, {rt, Section, Project}).

init_train_op(Runtime, Section, Project) ->
    e2_service:call(Runtime, {init_train_op, Section, Project}).

init_eval_op(Runtime, Run, Section, Project) ->
    e2_service:call(Runtime, {init_eval_op, Run, Section, Project}).

%% ===================================================================
%% Dispatch
%% ===================================================================

handle_msg({rt, Section, Project}, _From, State) ->
    {Result, Next} = runtime_for_section(Section, Project, State),
    {reply, Result, Next}.

%% ===================================================================
%% Find runtime
%% ===================================================================

runtime_for_section(Section, Project, State) ->
    maybe_runtime(runtime_mod_for_op(Section, Project), State).

runtime_mod_for_op(Section, Project) ->
    runtime_mod_for_attr(find_runtime_attr(Section, Project), Section).

find_runtime_attr(Section, Project) ->
    hubc_util:find_apply(
      [fun() -> hubc_project:section_attr(Section, "runtime") end,
       fun() -> hubc_project:attr(Project, ["project"], "runtime") end],
      []).

runtime_mod_for_attr({ok, Val}, _Section) ->
    runtime_mod_for_name(Val);
runtime_mod_for_attr(error, {[Type], _}) ->
    {error, {no_runtime, Type}};
runtime_mod_for_attr(error, {[Type, Name|_], _}) ->
    {error, {no_runtime, Type, Name}}.

runtime_mod_for_name("tensorflow") -> {ok, hubc_tensorflow_runtime};
runtime_mod_for_name("test")       -> {ok, hubc_test_runtime};
runtime_mod_for_name(Name)         -> {error, {runtime, Name}}.

maybe_runtime({ok, Mod}, State) ->
    maybe_init_runtime(try_cache(Mod, State), Mod, State);
maybe_runtime({error, Err}, State) ->
    {{error, Err}, State}.

try_cache(Mod, #state{cache=C}) -> dict:find(Mod, C).

maybe_init_runtime({ok, Cached}, _Mod, State) ->
    {{ok, Cached}, State};
maybe_init_runtime(error, Mod, State) ->
    {ok, Pid} = hubc_runtime_sup:start_child(Mod),
    {{ok, Pid}, cache_runtime(Mod, Pid, State)}.

cache_runtime(Mod, Pid, #state{cache=C}=S) ->
    S#state{cache=dict:store(Mod, Pid, C)}.
