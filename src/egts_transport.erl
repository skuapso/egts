-module(egts_transport).

-export([
  parse/1,
  response/1,
  packet_type/1
  ]).

-include_lib("logger/include/log.hrl").
-include("egts_binary_types.hrl").

packet_type(0) -> egts_pt_response;
packet_type(1) -> egts_pt_appdata;
packet_type(2) -> egts_pt_signed_appdata.

response({PID, Data}) ->
  SFRD = iolist_to_binary([<<PID:?USHORT, 0:?BYTE>> | Data]),
%  SFRD = <<PID:?USHORT, 0:?BYTE>>,
  PRV = 16#01,
  SKID = 0,
  PRF = 2#00,
  RTE = 0,
  ENA = 0,
  CMP = 0,
  PR = 2#00,
  HL = 16#0b,
  HE = 0,
  FDL = byte_size(SFRD),
  PT = 0,
  Header = <<
      PRV:?BYTE, SKID:?BYTE,
      PRF:2, RTE:1, ENA:2, CMP:1, PR:2,
      HL:?BYTE,
      HE:?BYTE,
      FDL:?USHORT, PID:?USHORT, PT:?BYTE>>,
  debug("header of response ~w", [Header]),
  HCS = egts:crc8(Header),
  ServiceData = case SFRD of
    <<>> -> <<>>;
    _    -> SFRCS = egts:crc16(SFRD),
      <<SFRD/binary, SFRCS:?USHORT>>
  end,
  <<Header/binary, HCS:?BYTE, ServiceData/binary>>.

parse(<<
        PRV:?BYTE, SKID:?BYTE,
        PRF:2, 0:1, ENA:2, CMP:1, PR:2,
        HL:?BYTE,
        HE:?BYTE,
        FDL:?USHORT, PID:?USHORT, PT:?BYTE,
        _/binary
      >> = Data
     )
    when
    byte_size(Data) =:= (HL + FDL + 2),
    PRV =:= 16#01, PRF =:= 2#00, HL =:= 16#0b,
    % не определены в данной версии протокола
    ENA =:= 0, CMP =:= 0, SKID =:= 0, HE =:= 0
    ->
  PacketType = packet_type(PT),
  <<Header:16#0a/binary, HCS:?BYTE, _/binary>> = Data,
  debug("header: ~s, crc: ~w", [Header, HCS]),
  true = egts:check_crc8(HCS, Header),
  {SFRD, Else} = get_service_data(FDL, Data, 16#0b),
  {
    [{PacketType, Data, [{priority, PR}, {msg_id, PID}, {service_frame, SFRD}]}],
    Data,
    Else
  };
parse(<<
        PRV:?BYTE, SKID:?BYTE,
        PRF:2, 1:1, ENA:2, CMP:1, PR:2,
        HL:?BYTE,
        HE:?BYTE,
        FDL:?USHORT, PID:?USHORT, _PT:?BYTE,
        PRA:?USHORT, RCA:?USHORT, TTL:?BYTE,
        _/binary
      >> = Data
     )
    when
    byte_size(Data) =:= (HL + FDL + 2),
    PRV =:= 16#01, PRF =:= 2#00, HL =:= 16#10,
    % не определены в данной версии протокола
    ENA =:= 0, CMP =:= 0, SKID =:= 0, HE =:= 0
    ->
  {SFRD, Else} = get_service_data(FDL, Data, 16#10),
  {
    [{
        egts_pt_routing,
        Data,
        {PRA, RCA, TTL, [{priority, PR}, {msg_id, PID}, {service_frame, SFRD}]}
    }],
    Data,
    Else
  };
parse(<<_:24, HL:?BYTE, _:8, FDL:?USHORT, _/binary>> = Data)
    when FDL > 0, ((HL =:= 16#10) or (HL =:= 16#0b)), byte_size(Data) < (HL + FDL + 2) ->
  debug("incomplete data ~w, header length ~w, frame length ~w", [Data, HL, FDL]),
  {[], <<>>, Data};
parse(<<_:24, HL:?BYTE, _:8, FDL:?USHORT, _/binary>> = Data)
    when FDL =:= 0, ((HL =:= 16#10) or (HL =:= 16#0b)), byte_size(Data) < HL ->
  debug("incomplete data ~w, header length ~w, frame length ~w", [Data, HL, FDL]),
  {[], <<>>, Data};
parse(<<_:24, HL:?BYTE, _/binary>> = Data)
    when HL =:= 16#10; HL =:= 16#0b ->
  parse(Data, <<>>, []);
parse(<<
        PRV:?BYTE, SKID:?BYTE,
        PRF:2, 0:1, ENA:2, CMP:1, PR:2,
        HL:?BYTE,
        HE:?BYTE,
        FDL:?USHORT, PID:?USHORT, PT:?BYTE,
        _/binary
      >> = Data
     ) when HL =/= 16#0a, HL =/= 16#10 ->
  warning("wrong length; got ~w(~w/~w)", [byte_size(Data), {header, HL}, {service, FDL}]),
  debug("prv:  ~w(~w)", [PRV, 16#01]),
  debug("skid: ~w(~w)", [SKID, {undef, 0}]),
  debug("prf:  ~w(~w)", [PRF, 2#00]),
  debug("ena:  ~w(~w)", [ENA, {undef, 0}]),
  debug("cmp:  ~w(~w)", [CMP, {undef, 0}]),
  debug("pr:   ~w(~w)", [PR, {priority}]),
  debug("hl:   ~w(~w)", [HL, {header_length, 16#10, 16#0b}]),
  debug("he:   ~w(~w)", [HE, {undef, 0}]),
  debug("fdl:  ~w(~w)", [FDL, {service_frame_length}]),
  debug("pid:  ~w(~w)", [PID, {msg_id}]),
  debug("pt:   ~w(~w)", [PT, {packet_type, 0, 1, 2}]),
  {badlen, {data, byte_size(Data)}, {header, HL}, {frame, FDL}};
parse(Data) -> {[], <<>>, Data}.

parse(<<>>, Raw, Parsed) ->
  {lists:reverse(Parsed), Raw, <<>>};
parse(Data, Raw, Parsed) ->
  {Data1, Rest} = get_transport_data(Data),
  case parse(Data1) of
    {[], <<>>, Data1} ->
      {lists:reverse(Parsed), Raw, Data1};
    {[P], R, <<>>} ->
      parse(Rest, <<Raw/binary, R/binary>>, [P | Parsed])
  end.

get_service_data(L, Data, Offset) ->
  <<_:Offset/binary, Else/binary>> = Data,
  get_service_data(L, Else).

get_service_data(0, Else) ->
  {<<>>, Else};
get_service_data(L, Data) ->
  <<SFRD:L/binary, SFRCS:?USHORT, Else/binary>> = Data,
  debug("service data: ~s, crc: ~w", [SFRD, SFRCS]),
  true = egts:check_crc16(SFRCS, SFRD),
%  {ok, Records} = egts_service:parse(SFRD),
%  debug("records ~w", [Records]),
%  {Records, Else}.
  {SFRD, Else}.

get_transport_data(<<_:24, HL:?BYTE, _:8, FDL:?USHORT, _/binary>> = Data)
  when FDL > 0, byte_size(Data) >= (HL + FDL + 2) ->
  L = HL + FDL + 2,
  <<TD:L/binary, Rest/binary>> = Data,
  {TD, Rest};
get_transport_data(<<_:24, HL:?BYTE, _:8, 0:?USHORT, _/binary>> = Data)
  when byte_size(Data) >= HL ->
  <<TD:HL/binary, Rest/binary>> = Data,
  {TD, Rest};
get_transport_data(Data) ->
  {<<>>, Data}.
