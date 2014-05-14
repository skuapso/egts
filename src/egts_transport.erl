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

parse(<<_:24, HL:?BYTE, _:8, FDL:?USHORT, _/binary>> = Data)
    when FDL =:= 0, byte_size(Data) < HL ->
  debug("incomplete data ~w, header length ~w, frame length ~w", [Data, HL, FDL]),
  {[], <<>>, Data};
parse(<<_:24, HL:?BYTE, _:8, FDL:?USHORT, _/binary>> = Data)
    when byte_size(Data) < (HL + FDL + 2) ->
  debug("incomplete data ~w, header length ~w, frame length ~w", [Data, HL, FDL]),
  {[], <<>>, Data};
parse(<<
        PRV:?BYTE, SKID:?BYTE,
        PRF:2, 1:1, ENA:2, CMP:1, PR:2,
        16#10:?BYTE, % HL
        HE:?BYTE,
        FDL:?USHORT, PID:?USHORT, _PT:?BYTE,
        PRA:?USHORT, RCA:?USHORT, TTL:?BYTE,
        _/binary
      >> = Data
     )
    when
    PRV =:= 16#01, PRF =:= 2#00,
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
parse(<<
        PRV:?BYTE, SKID:?BYTE,
        PRF:2, 0:1, ENA:2, CMP:1, PR:2,
        16#0b:?BYTE, % HL
        HE:?BYTE,
        FDL:?USHORT, PID:?USHORT, PT:?BYTE,
        _/binary
      >> = Data
     )
    when
    PRV =:= 16#01, PRF =:= 2#00,
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
parse(Data) ->
  warning("unparsable data: ~w", [Data]).

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
