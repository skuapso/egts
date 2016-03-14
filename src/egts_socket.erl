%%%-------------------------------------------------------------------
%%% @author Ilya Ashchepkov
%%% @copyright 2014 NskAvd
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(egts_socket).

-behaviour(gen_server).

%% API
-export([accept/1]).
-export([send/2]).
-export([connect/3]).
-export([setopts/2]).
-export([recv/2]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {socket, handler, incomplete = <<>>, address, ttl = 10}).

-include("egts_binary_types.hrl").
-include_lib("logger/include/log.hrl").

%%%===================================================================
%%% API
%%%===================================================================
accept(Socket) ->
  State = #state{socket = Socket, handler = self()},
  Reply = {ok, Pid} = gen_server:start_link(?MODULE, State, []),
  gen_tcp:controlling_process(Socket, Pid),
  Reply.

send(Pid, Data) when is_pid(Pid) ->
  gen_server:cast(Pid, {send, self(), Data}).

connect(Host, Port, Opts) ->
  State = #state{socket = {Host, Port, Opts}, handler = self()},
  gen_server:start_link(?MODULE, State, []).

setopts(Pid, Opts) ->
  gen_server:cast(Pid, {setopts, Opts}).

recv(Pid, L) ->
  gen_server:call(Pid, {recv, L}).
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

init(#state{socket = Socket} = State) when is_port(Socket) ->
  '_trace'("init"),
  egts_reg:register(Socket, self()),
  process_flag(trap_exit, true),
  {ok, State};

init(#state{socket = {Host, Port, Opts}} = State) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, Opts),
  init(State#state{socket = Socket}).


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
handle_call({recv, 0}, _From, #state{socket = Socket, address = Address} = State) ->
  Reply = recv([], Socket, Address, <<>>),
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  '_warning'("unhandled call ~w from ~w", [_Request, _From]),
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
handle_cast({send, _From, Data}, #state{socket = Socket} = State) when is_binary(Data) ->
  gen_tcp:send(Socket, Data),
  {noreply, State};
handle_cast({send, From, Data}, #state{
                             ttl = Ttl,
                             handler = From} = State) ->
  Data1 = case maps:get(ttl, Data, undefined) of
            undefined -> Data#{ttl => Ttl};
            _ -> Data
          end,
  {ok, BinData} = pack(Data1),
  handle_cast({send, From, BinData}, State);
handle_cast({setopts, Opts}, #state{socket = Socket} = State) ->
  inet:setopts(Socket, Opts),
  {noreply, State};
handle_cast(_Msg, State) ->
  '_warning'("unhandled cast ~w", [_Msg]),
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
handle_info({tcp, Socket, TcpData}, #state{
                                    socket = Socket,
                                    incomplete = Incomplete,
                                    address = Address,
                                    handler = Handler} = State)
  when (byte_size(TcpData) + byte_size(Incomplete)) =< 65535->
  Data = <<Incomplete/binary, TcpData/binary>>,
  '_trace'("parsing ~w", [Data]),
  {Packets, Incomplete1} = unpack([], Data, Address),
  lists:map(
    fun(Packet) ->
        egts:handle_data(Handler, Socket, Packet)
    end,
    Packets),
  {noreply, State#state{incomplete = Incomplete1}};
handle_info({tcp_closed, Socket}, #state{handler = Handler, socket = Socket} = State) ->
  egts:closed(Handler, Socket),
  {noreply, State};
handle_info({'EXIT', Handler, _Reason}, #state{handler = Handler} = State) ->
  {stop, normal, State};
handle_info({tcp, Socket, _}, #state{socket = Socket} = State) ->
  {stop, overflow, State};
handle_info(_Info, State) ->
  '_warning'("unhandled info msg ~w", [_Info]),
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
terminate(Reason, _State) ->
  '_warning'(Reason =/= normal, "terminating with reason ~w", [Reason], debug),
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
  '_notice'("code change from ~w with extra ~w", [_OldVsn, _Extra]),
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
recv(P, Socket, Address, Incomplete) ->
  {ok, Data} = gen_tcp:recv(Socket, 0),
  '_debug'("recieved ~p (~p bytes)", [Data, byte_size(Data)]),
  case unpack([], <<Incomplete/binary, Data/binary>>, Address) of
    {Packets, <<>>} -> {ok, P ++ Packets};
    {Packets, Inc} ->
      recv(P ++ Packets, Socket, Address, <<Incomplete/binary, Inc/binary>>)
  end.

pack(Data) when is_binary(Data) ->
  {ok, Data};
pack(#{header := Header, header_crc := HeadCrc, frame := Frame}) ->
  pack(<<Header/binary, HeadCrc:?BYTE, Frame/binary>>);
pack(Data) when is_map(Data) ->
  %% predefined
  PRV = 16#01,  % protocol version
  SKID = 0,     % not defined
  PRF = 2#00,   % header prefix (header version?)
  ENA = 0,      % not defined
  CMP = 0,      % not defined
  HE = 0,       % not defined

  PID = maps:get(packet_id, Data),
  Frame = maps:get(frame, Data),
  Type = maps:get(type, Data),
  PR = maps:get(priority, Data),    % priority
  FDL = case byte_size(Frame) of
          0 -> 0;
          L when L > 2 -> L - 2
        end,
  PT = packet_type(Type),
  From = maps:get(from, Data, undefined),
  To = maps:get(to, Data, undefined),
  TTL = maps:get(ttl, Data),
  {RTE, HeaderAdd} = case {From, To} of
                       {undefined, undefined} -> {0, <<>>};
                       {PRA, RCA} -> {1, <<PRA:?USHORT,
                                           RCA:?USHORT,
                                           TTL:?BYTE>>}
           end,
  HL = 16#0b + byte_size(HeaderAdd), % header length
  Header = <<PRV:?BYTE, SKID:?BYTE,
             PRF:2, RTE:1, ENA:2, CMP:1, PR:2,
             HL:?BYTE,
             HE:?BYTE,
             FDL:?USHORT, PID:?USHORT, PT:?BYTE,
             HeaderAdd/binary>>,
  HCS = egts:crc8(Header),
  pack(<<Header/binary, HCS:?BYTE, Frame/binary>>).

unpack(Packets,
       <<
         PRV:?BYTE, SKID:?BYTE,
         PRF:2, RTE:1, ENA:2, CMP:1, PR:2,
         HL:?BYTE,
         HE:?BYTE,
         FDL:?USHORT,
         PID:?USHORT,
         PT:?BYTE,
         _/binary
       >> = Data,
      Address)
  when
    PRV =:= 16#01         % protocol version
    andalso PRF =:= 2#00  % header prefix (header version?)
    andalso (
     (RTE =:= 0 andalso HL =:= 16#0b)
     orelse HL =:= 16#10
    )
    % не определены в данной версии протокола
    andalso ENA =:= 0
    andalso CMP =:= 0
    andalso SKID =:= 0
    andalso HE =:= 0
    ->
  print_header(false, Data),
  HeadLen = HL - 1,
  FrameLen = case FDL of
               0 -> 0;
               L -> L + 2
             end,
  case byte_size(Data) < (HL + FrameLen) of
    true -> {lists:reverse(Packets), Data};
    false ->
      <<Header:HeadLen/binary, HeadCrc:8, FrameWithCrc:FrameLen/binary, Rest/binary>> = Data,
      print_header(false, Header),
      HeadCrc = egts:crc8(Header),
      case FrameWithCrc of
        <<>> -> ok;
        <<F:FDL/binary, FrameCrc:?USHORT>> ->
          FrameCrc = egts:crc16(F)
      end,
      {PacketType, PRA, RCA, TTL} = case RTE of
                                      0 ->
                                        {packet_type(PT), undefined, undefined, undefined};
                                      1 ->
                                        <<_:10/binary,
                                          PRA_:?USHORT,
                                          RCA_:?USHORT,
                                          TTL_:?BYTE>> = Header,
                                        {egts_pt_routing, PRA_, RCA_, TTL_}
                                    end,
      EgtsData = #{priority => PR,
                   version => PRV,
                   from => PRA,
                   to => RCA,
                   ttl => TTL,
                   header => Header,
                   header_crc => HeadCrc,
                   frame => FrameWithCrc,
                   type => PacketType,
                   packet_id => PID
                  },
      unpack([EgtsData | Packets], Rest, Address)
  end;
unpack(Packets, Data, _Address) when byte_size(Data) < 10 ->
  {lists:reverse(Packets), Data};
unpack(Packets, <<Header:16#0a/binary, _/binary>>, _Address) ->
  '_warning'("wrong header format"),
  print_header(true, Header),
  {lists:reverse(Packets), <<>>};
unpack(Packets, Data, _Address) ->
  '_warning'("wrong data ~p", [Data]),
  {lists:reverse(Packets), <<>>}.

packet_type(egts_pt_response) -> 0;
packet_type(egts_pt_appdata)  -> 1;
packet_type(egts_pt_signed_appdata) -> 2;
packet_type(0) -> egts_pt_response;
packet_type(1) -> egts_pt_appdata;
packet_type(2) -> egts_pt_signed_appdata.

print_header(Condition,
             <<
               PRV:?BYTE, SKID:?BYTE,
               PRF:2, RTE:1, ENA:2, CMP:1, PR:2,
               HL:?BYTE,
               HE:?BYTE,
               FDL:?USHORT, PID:?USHORT, PT:?BYTE,
               _/binary
             >> = Data
            ) ->
  '_warning'(Condition, "prv:  ~w(~w)", [PRV, 16#01], debug),
  '_warning'(Condition, "skid: ~w(~w)", [SKID, {undef, 0}], debug),
  '_warning'(Condition, "prf:  ~w(~w)", [PRF, 2#00], debug),
  '_warning'(Condition, "prf:  ~w(~w)", [RTE, {0, 1}], debug),
  '_warning'(Condition, "ena:  ~w(~w)", [ENA, {undef, 0}], debug),
  '_warning'(Condition, "cmp:  ~w(~w)", [CMP, {undef, 0}], debug),
  '_warning'(Condition, "pr:   ~w(~w)", [PR, {priority}], debug),
  '_warning'(Condition, "hl:   ~w(~w)", [HL, {header_length, 16#10, 16#0b}], debug),
  '_warning'(Condition, "he:   ~w(~w)", [HE, {undef, 0}], debug),
  '_warning'(Condition, "fdl:  ~w(~w)", [FDL, {service_frame_length}], debug),
  '_warning'(Condition, "pid:  ~w(~w)", [PID, {packet_id}], debug),
  '_warning'(Condition, "pt:   ~w(~w)", [PT, {packet_type, 0, 1, 2}], debug),
  '_warning'(Condition, "data len: ~w", [byte_size(Data)], debug).


%% vim: ft=erlang
