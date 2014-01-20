-module(egts_terminator).

-behaviour(gen_server).
-behaviour(terminator).

-export([
  parse/1,
  response/1
  ]).

%% terminator API
-export([
  start_link/1,
  close/1,
  accept/1,
  send/2,
  packet_type/1
  ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {socket, handler, incomplete = <<>>}).

-define(MAX_IN_SIZE, 65535).
-define(MAX_OUT_SIZE, 1400).
-define(TCP_OPTIONS, [{active, true}]).
-define(T, egts).
-define(TR, egts_transport).
-define(SF, egts_service).

-include_lib("logger/include/log.hrl").

-include("egts_binary_types.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Socket) ->
  gen_server:start_link(?MODULE, {Socket, self()}, []).

close(Pid) ->
  gen_server:cast(Pid, close).

accept(Pid) ->
  gen_server:cast(Pid, accept).

send(Pid, Data) ->
  gen_server:cast(Pid, {send, Data}).

parse({transport, Data}) ->
  ?TR:parse(Data);
parse({service, Data}) ->
  ?SF:parse(Data).

response({transport, Data}) ->
  ?TR:response(Data);
response({service, Data}) ->
  ?SF:response(Data).

packet_type(N) ->
  ?TR:packet_type(N).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init({Socket, Handler}) ->
  trace("starting"),
  {ok, #state{socket = Socket, handler = Handler}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Request, From, State) ->
  warning("unhandled call ~w from ~w", [Request, From]),
  close(),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(accept, #state{socket = Socket} = State) ->
  trace("accepting"),
  ok = inet:setopts(Socket, ?TCP_OPTIONS),
  {noreply, State};
handle_cast({send, <<>>}, State) ->
  {noreply, State};
handle_cast({send, Data}, State) when byte_size(Data) > ?MAX_OUT_SIZE ->
  <<Packet:?MAX_OUT_SIZE/binary, Else/binary>> = Data,
  handle_cast({send, Packet}, State),
  handle_cast({send, Else}, State);
handle_cast({send, Data}, #state{socket = Socket} = State) ->
  debug("sending ~w", [Data]),
  gen_tcp:send(Socket, Data),
  {noreply, State};
handle_cast(close, #state{incomplete = Incomplete} = State) when Incomplete =/= <<>> ->
  notice("request for close"),
  {stop, {incomplete, Incomplete}, State#state{incomplete = <<>>}};
handle_cast(close, #state{incomplete = Incomplete} = State) when Incomplete =:= <<>> ->
  trace("request for close"),
  {stop, normal, State};
handle_cast(Msg, State) ->
  warning("unhandled cast msg ~w", [Msg]),
  close(),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, Socket, TcpData}, #state{socket = Socket, incomplete = StoredData} = S)
    when (byte_size(TcpData) + byte_size(StoredData)) > ?MAX_IN_SIZE
    ->
  warning("overflow: ~w", [<<StoredData/binary, TcpData/binary>>]),
  close(),
  {noreply, S#state{incomplete = <<StoredData/binary, TcpData/binary>>}};
handle_info({tcp, Socket, TcpData}, #state{
        socket = Socket,
        handler = Handler,
        incomplete = StoredData} = S) ->
  trace("new tcp data"),
  RawData = <<StoredData/binary, TcpData/binary>>,
  debug("parsing ~w", [RawData]),
  {Packets, Parsed, Incomplete} = parse({transport, RawData}),
  debug("packets: ~w", [Packets]),
  debug("incomlete: ~w", [Incomplete]),
  ?T:data(Handler, Packets, Parsed),
  {noreply, S#state{incomplete = Incomplete}};
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
  trace("connection closed"),
  close(),
  {noreply, State};
handle_info(Info, State) ->
  warning("unhandled info ~w", [Info]),
  close(),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{socket = Socket}) when is_port(Socket) ->
  gen_tcp:close(Socket),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
close() ->
  close(self()).
