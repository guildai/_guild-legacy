-module(hubc_log_http).

-export([create_app/1, handle/2]).

-record(state, {start, method, path}).

create_app(Upstream) -> {?MODULE, handle, [Upstream]}.

handle(Upstream, Env0) ->
    {State, Env} = init_state(Env0),
    handle_app_result(catch psycho:call_app(Upstream, Env), State).

init_state(Env) ->
    Method = psycho:env_val(request_method, Env),
    Path = psycho:env_val(request_path, Env),
    Start = erlang:timestamp(),
    {#state{start=Start, method=Method, path=Path}, Env}.

handle_app_result(Result, State) ->
    #state{start=Start, method=Method, path=Path} = State,
    Date = psycho_datetime:iso8601(Start),
    Time = request_time(Start),
    Status = try_response_status(Result),
    io:format(
      standard_error,
      "[~s] ~s ~s ~b ~b~n",
      [Date, Method, Path, Status, Time]),
    Result.

request_time(Start) ->
    micro(erlang:timestamp()) - micro(Start).

micro({M, S, U}) ->
    M * 1000000000000 + S * 1000000 + U.

try_response_status({{Status, _}, _, _}) -> Status;
try_response_status(_) -> 0.
