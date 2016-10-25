%% Copyright 2106 TensorHub, Inc.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(guild_tensorflow_port).

-behavior(e2_service).

-export([start_link/0, load_image/2, run_model/2]).

-export([init/1, handle_msg/3]).

-record(state, {exec_pid, exec_ospid, callers, buf}).

%% ===================================================================
%% Start / init
%% ===================================================================

start_link() ->
    e2_service:start_link(?MODULE, [], [registered]).

init([]) ->
    process_flag(trap_exit, true),
    Exec = start_exec(),
    {ok, init_state(Exec)}.

start_exec() ->
    Args = [port_exe()],
    Opts = [stdout, stderr, stdin],
    {ok, Pid, OSPid} = guild_exec:run_link(Args, Opts),
    {Pid, OSPid}.

port_exe() ->
    guild_app:priv_bin("tensorflow-port").

init_state({ExecPid, ExecOSPid}) ->
    #state{
       callers=[],
       buf=[],
       exec_pid=ExecPid,
       exec_ospid=ExecOSPid}.

%% ===================================================================
%% API
%% ===================================================================

load_image(RunDir, Index) ->
    e2_service:call(?MODULE, {call, {load_image, RunDir, Index}}).

run_model(ModelPath, Request) ->
    e2_service:call(?MODULE, {call, {run_model, ModelPath, Request}}).

%% ===================================================================
%% Message dispatch
%% ===================================================================

handle_msg({call, Call}, From, State) ->
    handle_port_call(Call, From, State);
handle_msg({stdout, OSPid, Bin}, noreply, #state{exec_ospid=OSPid}=State) ->
    handle_stdout(Bin, State);
handle_msg({stderr, OSPid, Bin}, noreply, #state{exec_ospid=OSPid}=State) ->
    handle_stderr(Bin, State);
handle_msg({'EXIT', Pid, Reason}, noreply, #state{exec_pid=Pid}=State) ->
    handle_exec_exit(Reason, State).

%% ===================================================================
%% Port call
%% ===================================================================

handle_port_call(Call, From, State) ->
    Ref = dispatch_port_call(Call, State),
    Next = add_caller(From, Ref, Call, State),
    {noreply, Next}.

dispatch_port_call(Call, #state{exec_ospid=OSPid}) ->
    Ref = request_ref(),
    Request = encode_request(Ref, Call),
    ok = exec:send(OSPid, Request),
    Ref.

request_ref() -> integer_to_binary(rand:uniform(1000000)).

encode_request(Ref, Call) ->
    iolist_to_binary([Ref, $\t, encode_call(Call), $\n]).

encode_call({load_image, Dir, Index}) ->
    ["load-image", $\t, Dir, $\t, integer_to_list(Index)];
encode_call({run_model, ModelPath, Request}) ->
    ["run-model", $\t, ModelPath, $\t, encode_json_arg(Request)].

encode_json_arg(JSON) ->
    re:replace(JSON, "[\n\r\t]", " ", [global]).

add_caller(From, Ref, Call, #state{callers=Callers}=S) ->
    S#state{callers=[{Ref, Call, From}|Callers]}.

%% ===================================================================
%% Stdout
%% ===================================================================

handle_stdout(Bin, State) ->
    handle_split_stdout(binary:split(Bin, <<"\n\n">>), State).

handle_split_stdout([Part], State) ->
    {noreply, buffer(Part, State)};
handle_split_stdout([EOF, Rest], State) ->
    handle_response(buffer(EOF, State), Rest).

buffer(Bin, #state{buf=Buf}=S) -> S#state{buf=[Bin|Buf]}.

handle_response(#state{callers=[{Ref, Call, From}|Rest], buf=Buf}=S, NextBuf) ->
    {Ref, Resp} = decode_response(lists:reverse(Buf), Call),
    e2_service:reply(From, Resp),
    {noreply, S#state{callers=Rest, buf=new_buf(NextBuf)}}.

decode_response(Buf, Call) ->
    [Line|Parts] = (re:split(Buf, "\n")),
    [Ref, StatusBin] = re:split(Line, "\t"),
    Status = binary_to_existing_atom(StatusBin, latin1),
    {Ref, decode_call_result(Status, Call, Parts)}.

new_buf(<<>>) -> [];
new_buf(Next) -> [Next].

%% ===================================================================
%% Response decoders
%% ===================================================================

decode_call_result(ok, {load_image, _, _}, Parts) ->
    {ok, decode_image(Parts)};
decode_call_result(ok, {run_model, _, _}, Parts) ->
    {ok, decode_model_run_response(Parts)};
decode_call_result(error, _, Error) ->
    {error, decode_error(Error)}.

decode_image([File, Tag, EncDim, Type, EncBytes]) ->
    Dim = decode_image_dim(EncDim),
    Bytes = decode_image_bytes(EncBytes),
    #{file => File,
      tag => Tag,
      dim => Dim,
      type => Type,
      bytes => Bytes}.

decode_image_dim(Enc) ->
    [H, W, D] = re:split(Enc, " ", []),
    {binary_to_integer(H), binary_to_integer(W), binary_to_integer(D)}.

decode_image_bytes(Enc) -> base64:decode(Enc).

decode_model_run_response(Parts) ->
    Parts.

decode_error(Msg) -> iolist_to_binary(Msg).

%% ===================================================================
%% Stderr
%% ===================================================================

handle_stderr(<<"I ", _/binary>>, State) ->
    %% Ignore information logging from TensorFlow
    {noreply, State};
handle_stderr(Bin, State) ->
    io:format(standard_error, "~s", [Bin]),
    {noreply, State}.

%% ===================================================================
%% Exec exited
%% ===================================================================

handle_exec_exit(normal, State) ->
    {stop, normal, State};
handle_exec_exit({exit_status, Status}, State) ->
    {stop, {exec_exit, exec:status(Status)}, State}.
