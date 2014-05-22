-module(egts).

-behaviour(gen_server).
-behaviour(terminator).

%% API
-export([
  data/3,
  crc8/1,
  crc16/1,
  check_crc8/2,
  check_crc16/2
  ]).

%% terminator API
-export([
  close/1,
  accept/1,
  enter_loop/1,
  packet_type/1
  ]).

%% gen_server callbacks
-export([
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(TIMEOUT, 300000).
-define(T, egts_terminator).

-record(state, {
    terminator,
    uin,
    timeout = ?TIMEOUT,
    answer = <<>>,
    response = <<>>,
    packet_id,
    local_address = 0
    }).

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
accept(Pid) ->
  gen_server:cast(Pid, accept).

close(Pid) ->
  gen_server:cast(Pid, close).

enter_loop(Socket) ->
  gen_server:enter_loop(?MODULE, [], Socket, ?TIMEOUT).

data(_Pid, [], <<>>) ->
  ok;
data(Pid, Packets, RawData) ->
  gen_server:call(Pid, {raw_data, RawData}, ?TIMEOUT),
  data(Pid, Packets).

data(Pid, []) ->
  gen_server:call(Pid, {data, []}, ?TIMEOUT);
data(Pid, [Packet | Packets]) ->
  gen_server:call(Pid, Packet, ?TIMEOUT),
  data(Pid, Packets).

packet_type(N) ->
  ?T:packet_type(N).

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
handle_call({raw_data, RawData}, _From, #state{uin = undefined} = State) ->
  {reply, ok, State#state{answer = RawData}};
handle_call({raw_data, RawData}, {TPid, _Tag},
            #state{timeout = Timeout, uin = UIN, terminator = TPid} = State) ->
  Answer = case hooks:run(terminal_raw_data, [?MODULE, UIN, RawData], Timeout) of
    [] ->
      <<>>;
    [{Module, {answer, RawAnswer}}] ->
      {Module, RawAnswer}
  end,
  {reply, ok, State#state{answer = Answer}, Timeout};
handle_call({data, []}, {TPid, _Tag}, #state{timeout = Timeout,
                                             terminator = TPid,
                                             uin = UIN,
                                             answer = {Module, Answer}} = State) ->
  debug("terminal answer ~w", [{Module, Answer}]),
  hooks:run(terminal_answer, [Module, UIN, Answer], Timeout),
  ?T:send(TPid, Answer),
  {reply, ok, State#state{answer = <<>>, response = <<>>}, Timeout};
%%--------------------------------------------------------------------
%% in this case answer should be determinated by module from the internal state
%%--------------------------------------------------------------------
handle_call({data, []} = Data, From, #state{response = Answer} = State) ->
  handle_call(Data, From, State#state{answer = {?MODULE, Answer}});
%%--------------------------------------------------------------------
%% in this case should process parsed data
%%--------------------------------------------------------------------
handle_call({egts_pt_routing = Type, RawData, {_PRA, _RCA, _TTL, Data}} = Packet,
            From,
            State) ->
  emerg("~w ~w", [Type, Packet]),
  <<_:9/binary, PT:?BYTE, _/binary>> = RawData,
  PacketType = ?T:packet_type(PT),
  handle_call({PacketType, RawData, Data}, From, State);
handle_call({egts_pt_response = Type, _Raw, Parsed}, _From, State) ->
  SF = proplists:get_value(service_frame, Parsed, <<>>),
  {ok, SR} = ?T:parse({service, egts_pt_response, SF}),
  warning("~w ~w", [Type, SR]),
  {reply, ok, State, State#state.timeout};
handle_call({Type, _RawData, Parsed},
            {TPid, _Tag},
            #state{timeout = Timeout, terminator = TPid} = State) ->
  trace("new data"),
  PacketID = proplists:get_value(msg_id, Parsed),
  ServiceFrame = proplists:get_value(service_frame, Parsed, <<>>),
  {ok, ServiceRecords} = ?T:parse({service, Type, ServiceFrame}),
  debug("service records: ~w", [ServiceRecords]),
  {Answers, NewState} = handle_service_records(ServiceRecords, State),
  debug("service reponses ~w", [Answers]),
  Response = ?T:response({transport, {PacketID, Answers}}),
  debug("response is ~w", [Response]),
  {reply, ok, NewState#state{response = Response}, Timeout};
handle_call(Request, From, #state{timeout = Timeout, terminator = TPid} = State) ->
  warning("unhandled call ~w from ~w", [Request, From]),
  ?T:close(TPid),
  {reply, ok, State, Timeout}.

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
handle_cast(accept, Socket) ->
  trace("accept"),
  {ok, TPid} = ?T:start_link(Socket),
  ok = gen_tcp:controlling_process(Socket, TPid),
  ok = ?T:accept(TPid),
  {noreply, #state{terminator = TPid}, ?TIMEOUT};
handle_cast(close, #state{terminator = TPid, timeout = Timeout} = State) ->
  trace("cast for close"),
  ?T:close(TPid),
  {noreply, State, Timeout};
handle_cast(Msg, #state{terminator = TPid, timeout = Timeout} = State) ->
  warning("unhandled cast: ~w", [Msg]),
  ?T:close(TPid),
  {noreply, State, Timeout}.

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
handle_info(timeout, #state{terminator = TPid, timeout = Timeout} = State) ->
  trace("timeout, closing"),
  ?T:close(TPid),
  {noreply, State, Timeout};
handle_info({'EXIT', From, normal}, #state{terminator = From} = State) ->
  trace("terminator died"),
  {stop, normal, State};
handle_info({'EXIT', From, Reason}, #state{terminator = From} = State) ->
  warning("terminator died ~w", [Reason]),
  {stop, Reason, State};
handle_info(Info, #state{terminator = TPid, timeout = Timeout} = State) ->
  warning("unhandled info ~w", [Info]),
  ?T:close(TPid),
  {noreply, State, Timeout}.

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
terminate(_Reason, _State) ->
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
handle_service_records(Services, State) ->
  handle_service_records(Services, [], State).

handle_service_records([], Responses, State) ->
  {lists:reverse(Responses), State};
handle_service_records([Service | Services], Responses, State) ->
  {Response, NewState} = handle_service_record(Service, State),
  handle_service_records(Services, [Response | Responses], NewState).

handle_service_record(Record, #state{uin = undefined,
                                     answer = RawData,
                                     terminator = TPid,
                                     timeout = Timeout} = State) ->
  UIN = case Record of
          {auth, _, [{_, [{auth, AuthData}]}], _} ->
            proplists:get_value(imei, AuthData,
                                proplists:get_value(terminal_id, AuthData));
          {_, _, _, Info} ->
            proplists:get_value(object_id, Info)
        end,
  true = is_integer(UIN),
  hooks:run(terminal_uin, [?MODULE, UIN], Timeout),
  {reply, ok, NewState, Timeout} = handle_call({raw_data, RawData}, {TPid, undefined},
                          State#state{uin = UIN, answer = <<>>}),
  handle_service_record(Record, NewState#state{timeout = Timeout});
handle_service_record({Type, _, Packets, Info} = Record, #state{uin = UIN,
                                                          timeout = Timeout} = State) ->
  debug("handling service record ~w", [Record]),
  [case Packet of
     {RawPacket, Parsed} ->
       Type1 = packet_type(Type, Parsed),
       debug("type: ~w", [Type1]),
       hooks:run(terminal_packet, [?MODULE, UIN, Type1, RawPacket, Parsed], Timeout);
     Else -> warning("packet wrong format ~w", [Else])
   end
   || Packet <- Packets],
  {?T:response({service, Info}), State}.

packet_type(auth, _) -> authentication;
packet_type(teledata, Data) ->
  Nav = proplists:get_value(navigation, Data),
  case proplists:get_value(offline, Nav) of
    1 -> offline;
    0 -> online
  end;
packet_type(_, _) -> unknown.

-define(CRC16Table, {
    16#0000, 16#1021, 16#2042, 16#3063, 16#4084, 16#50A5, 16#60C6, 16#70E7,
    16#8108, 16#9129, 16#A14A, 16#B16B, 16#C18C, 16#D1AD, 16#E1CE, 16#F1EF,
    16#1231, 16#0210, 16#3273, 16#2252, 16#52B5, 16#4294, 16#72F7, 16#62D6,
    16#9339, 16#8318, 16#B37B, 16#A35A, 16#D3BD, 16#C39C, 16#F3FF, 16#E3DE,
    16#2462, 16#3443, 16#0420, 16#1401, 16#64E6, 16#74C7, 16#44A4, 16#5485,
    16#A56A, 16#B54B, 16#8528, 16#9509, 16#E5EE, 16#F5CF, 16#C5AC, 16#D58D,
    16#3653, 16#2672, 16#1611, 16#0630, 16#76D7, 16#66F6, 16#5695, 16#46B4,
    16#B75B, 16#A77A, 16#9719, 16#8738, 16#F7DF, 16#E7FE, 16#D79D, 16#C7BC,
    16#48C4, 16#58E5, 16#6886, 16#78A7, 16#0840, 16#1861, 16#2802, 16#3823,
    16#C9CC, 16#D9ED, 16#E98E, 16#F9AF, 16#8948, 16#9969, 16#A90A, 16#B92B,
    16#5AF5, 16#4AD4, 16#7AB7, 16#6A96, 16#1A71, 16#0A50, 16#3A33, 16#2A12,
    16#DBFD, 16#CBDC, 16#FBBF, 16#EB9E, 16#9B79, 16#8B58, 16#BB3B, 16#AB1A,
    16#6CA6, 16#7C87, 16#4CE4, 16#5CC5, 16#2C22, 16#3C03, 16#0C60, 16#1C41,
    16#EDAE, 16#FD8F, 16#CDEC, 16#DDCD, 16#AD2A, 16#BD0B, 16#8D68, 16#9D49,
    16#7E97, 16#6EB6, 16#5ED5, 16#4EF4, 16#3E13, 16#2E32, 16#1E51, 16#0E70,
    16#FF9F, 16#EFBE, 16#DFDD, 16#CFFC, 16#BF1B, 16#AF3A, 16#9F59, 16#8F78,
    16#9188, 16#81A9, 16#B1CA, 16#A1EB, 16#D10C, 16#C12D, 16#F14E, 16#E16F,
    16#1080, 16#00A1, 16#30C2, 16#20E3, 16#5004, 16#4025, 16#7046, 16#6067,
    16#83B9, 16#9398, 16#A3FB, 16#B3DA, 16#C33D, 16#D31C, 16#E37F, 16#F35E,
    16#02B1, 16#1290, 16#22F3, 16#32D2, 16#4235, 16#5214, 16#6277, 16#7256,
    16#B5EA, 16#A5CB, 16#95A8, 16#8589, 16#F56E, 16#E54F, 16#D52C, 16#C50D,
    16#34E2, 16#24C3, 16#14A0, 16#0481, 16#7466, 16#6447, 16#5424, 16#4405,
    16#A7DB, 16#B7FA, 16#8799, 16#97B8, 16#E75F, 16#F77E, 16#C71D, 16#D73C,
    16#26D3, 16#36F2, 16#0691, 16#16B0, 16#6657, 16#7676, 16#4615, 16#5634,
    16#D94C, 16#C96D, 16#F90E, 16#E92F, 16#99C8, 16#89E9, 16#B98A, 16#A9AB,
    16#5844, 16#4865, 16#7806, 16#6827, 16#18C0, 16#08E1, 16#3882, 16#28A3,
    16#CB7D, 16#DB5C, 16#EB3F, 16#FB1E, 16#8BF9, 16#9BD8, 16#ABBB, 16#BB9A,
    16#4A75, 16#5A54, 16#6A37, 16#7A16, 16#0AF1, 16#1AD0, 16#2AB3, 16#3A92,
    16#FD2E, 16#ED0F, 16#DD6C, 16#CD4D, 16#BDAA, 16#AD8B, 16#9DE8, 16#8DC9,
    16#7C26, 16#6C07, 16#5C64, 16#4C45, 16#3CA2, 16#2C83, 16#1CE0, 16#0CC1,
    16#EF1F, 16#FF3E, 16#CF5D, 16#DF7C, 16#AF9B, 16#BFBA, 16#8FD9, 16#9FF8,
    16#6E17, 16#7E36, 16#4E55, 16#5E74, 16#2E93, 16#3EB2, 16#0ED1, 16#1EF0
    }).

-define(CRC8Table, {
    16#00, 16#31, 16#62, 16#53, 16#C4, 16#F5, 16#A6, 16#97,
    16#B9, 16#88, 16#DB, 16#EA, 16#7D, 16#4C, 16#1F, 16#2E,
    16#43, 16#72, 16#21, 16#10, 16#87, 16#B6, 16#E5, 16#D4,
    16#FA, 16#CB, 16#98, 16#A9, 16#3E, 16#0F, 16#5C, 16#6D,
    16#86, 16#B7, 16#E4, 16#D5, 16#42, 16#73, 16#20, 16#11,
    16#3F, 16#0E, 16#5D, 16#6C, 16#FB, 16#CA, 16#99, 16#A8,
    16#C5, 16#F4, 16#A7, 16#96, 16#01, 16#30, 16#63, 16#52,
    16#7C, 16#4D, 16#1E, 16#2F, 16#B8, 16#89, 16#DA, 16#EB,
    16#3D, 16#0C, 16#5F, 16#6E, 16#F9, 16#C8, 16#9B, 16#AA,
    16#84, 16#B5, 16#E6, 16#D7, 16#40, 16#71, 16#22, 16#13,
    16#7E, 16#4F, 16#1C, 16#2D, 16#BA, 16#8B, 16#D8, 16#E9,
    16#C7, 16#F6, 16#A5, 16#94, 16#03, 16#32, 16#61, 16#50,
    16#BB, 16#8A, 16#D9, 16#E8, 16#7F, 16#4E, 16#1D, 16#2C,
    16#02, 16#33, 16#60, 16#51, 16#C6, 16#F7, 16#A4, 16#95,
    16#F8, 16#C9, 16#9A, 16#AB, 16#3C, 16#0D, 16#5E, 16#6F,
    16#41, 16#70, 16#23, 16#12, 16#85, 16#B4, 16#E7, 16#D6,
    16#7A, 16#4B, 16#18, 16#29, 16#BE, 16#8F, 16#DC, 16#ED,
    16#C3, 16#F2, 16#A1, 16#90, 16#07, 16#36, 16#65, 16#54,
    16#39, 16#08, 16#5B, 16#6A, 16#FD, 16#CC, 16#9F, 16#AE,
    16#80, 16#B1, 16#E2, 16#D3, 16#44, 16#75, 16#26, 16#17,
    16#FC, 16#CD, 16#9E, 16#AF, 16#38, 16#09, 16#5A, 16#6B,
    16#45, 16#74, 16#27, 16#16, 16#81, 16#B0, 16#E3, 16#D2,
    16#BF, 16#8E, 16#DD, 16#EC, 16#7B, 16#4A, 16#19, 16#28,
    16#06, 16#37, 16#64, 16#55, 16#C2, 16#F3, 16#A0, 16#91,
    16#47, 16#76, 16#25, 16#14, 16#83, 16#B2, 16#E1, 16#D0,
    16#FE, 16#CF, 16#9C, 16#AD, 16#3A, 16#0B, 16#58, 16#69,
    16#04, 16#35, 16#66, 16#57, 16#C0, 16#F1, 16#A2, 16#93,
    16#BD, 16#8C, 16#DF, 16#EE, 16#79, 16#48, 16#1B, 16#2A,
    16#C1, 16#F0, 16#A3, 16#92, 16#05, 16#34, 16#67, 16#56,
    16#78, 16#49, 16#1A, 16#2B, 16#BC, 16#8D, 16#DE, 16#EF,
    16#82, 16#B3, 16#E0, 16#D1, 16#46, 16#77, 16#24, 16#15,
    16#3B, 16#0A, 16#59, 16#68, 16#FF, 16#CE, 16#9D, 16#AC
  }).

check_crc8(CRC, Bin) ->
  case crc8(Bin) of
    CRC -> true;
    _   -> false
  end.

crc8(Data) ->
  crc8(Data, 16#ff).

crc8(<<>>, CRC) ->
  CRC;
crc8(<<B:8, Else/binary>>, CRC) ->
  crc8(Else, element((CRC bxor B) + 1, ?CRC8Table)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Name : CRC-16 CCITT
%%  Poly : 0x1021 x^16 + x^12 + x^5 + 1
%%  Init : 0xFFFF
%%  Revert: false
%%  XorOut: 0x0000
%%  Check : 0x29B1 ("123456789")
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_crc16(CRC, Bin) ->
  case crc16(Bin, 16#FFFF) of
    CRC -> true;
    _   -> false
  end.

crc16(Data) ->
  crc16(Data, 16#ffff).

crc16(<<>>, CRC) ->
  CRC;
crc16(<<B:8, Else/binary>>, CRC) ->
  crc16(Else, ((CRC bsl 8) band 16#ffff) bxor
        element(((CRC bsr 8) bxor B) + 1, ?CRC16Table)).
%unsigned short Crc16(unsigned char * pcBlock, unsigned short len)
%    {
%      unsigned short crc = 0xFFFF;
%      while (len--)
%           crc = (crc << 8) ^ Crc16Table[(crc >> 8) ^ *pcBlock++];
%      return crc;
%    }

%unsigned char CRC8(unsigned char *lpBlock, unsigned char len)
%    {
%      unsigned char crc = 0xFF;
%      while (len--)
%           crc = CRC8Table[crc ^ *lpBlock++];
%      return crc;
%     }
