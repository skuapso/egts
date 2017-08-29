-module(egts_service).

-export([
  parse/2,
  response/1,
  service/1
  ]).

-include_lib("logger/include/log.hrl").
-include("egts_binary_types.hrl").

response(#{auth := #{status := Code}} = Info) ->
  RL = 4,
  RN = maps:get(msg_id, Info),
  RSOD = maps:get(rsod, Info),
  SSOD = maps:get(ssod, Info),
  GRP = maps:get(group_record, Info),
  RPP = maps:get(rpp, Info),
  Opts = 0,
  SST = service(auth),
  RST = service(auth),
  <<RL:?USHORT,
    RN:?USHORT,
    SSOD:1,
    RSOD:1,
    GRP:1,
    RPP:2,
    Opts:3,
    SST:8,
    RST:8,
    9:8,
    1:?USHORT,
    Code:?BYTE>>;
response(Info) ->
  RL = 6,
  RN = maps:get(msg_id, Info),
  RSOD = maps:get(rsod, Info),
  SSOD = maps:get(ssod, Info),
  GRP = maps:get(group_record, Info),
  RPP = maps:get(rpp, Info),
  Opts = 0,
  SST = service(maps:get(sst, Info)),
  RST = service(maps:get(rst, Info)),
  RecordStatus = 0,
  Record = <<RN:?USHORT, RecordStatus:?BYTE>>,
  <<RL:?USHORT,
    RN:?USHORT,
    SSOD:1,
    RSOD:1,
    GRP:1,
    RPP:2,
    Opts:3,
    SST:8,
    RST:8,
    0:8,
    3:?USHORT,
    Record/binary>>.

parse(response, Data) ->
  [{data, Data}];
parse(Type, Data) ->
  '_trace'("parsing ~w ~w", [Type, Data]),
  parse(Type, Data, [], []).

parse(_Type, <<>>, P, Infos) -> {lists:reverse(P), lists:reverse(Infos)};
parse(egts_pt_appdata = Type,
      <<RL:?USHORT, RN:?USHORT, SSOD:1, RSOD:1, GRP:1, RPP:2, Opts:3, _/binary>> = Data,
      Records, Infos) ->
  OptHL = header_length(Opts),
  '_debug'("record header length ~w", [OptHL]),
  '_debug'("record length ~w", [RL]),
  <<_:5/binary, OptHeader:OptHL/binary, SST:8, RST:8, RecordsBin:RL/binary, Else/binary>> = Data,
  '_debug'("header: ~w, opts: ~w", [OptHeader, Opts]),
  Info = parse_header(#{
           msg_id => RN,
           ssod => SSOD,
           rsod => RSOD,
           group_record => GRP,
           sst => service(SST),
           rst => service(RST),
           rpp => RPP},
                      Opts,
                      OptHeader),
  NewRecords = parse_records(Records, Info, RecordsBin),
  parse(Type, Else, NewRecords, [Info | Infos]);
parse(egts_pt_response = Type,
      <<RN:?USHORT,
        RecordStatus:?BYTE,
        Else/binary>>,
      Records, Infos) ->
  '_trace'("~w record number ~w", [Type, RN]),
  '_trace'("~w record status ~w", [Type, RecordStatus]),
  parse(egts_pt_appdata, Else, Records, Infos).

header_length(F) -> header_length(F, 0).
header_length(0, L) -> L * 4;
header_length(F, L) -> header_length(F bsr 1, F band 1 + L).

parse_records(P, _Info, <<>>) -> P;
parse_records(P, #{sst := SST} = Info, <<Type:?BYTE, L:?USHORT, Data:L/binary, Else/binary>>) ->
  '_trace'("subrecord ~w", [Data]),
  '_trace'("else ~w", [Else]),
  SR = subrecord(SST, Type),
  {Record, Parsed, Raw} = case {P, SR} of
                       {_, term_identify} ->
                         '_debug'("new auth record"),
                         {Info, P, <<>>};
                       {_, position} ->
                         '_debug'("new record"),
                         {Info, P, <<>>};
                       {[], _} ->
                         '_debug'("new record cause empty"),
                         {Info, P, <<>>};
                       {[Rec | Pars], _} ->
                         '_debug'("updating record ~w", [Rec]),
                         {Rec, Pars, maps:get(raw, Rec)}
                     end,
  Record1 = parse_subrecord(Record, SST, SR, Data),
  RecordType = case maps:get(offline, Record1, 1) of
           0 -> online;
           1 -> offline
         end,
  Record2 = Record1#{raw => <<Raw/binary, Type:?BYTE, L:?USHORT, Data/binary>>,
                     type => RecordType},
  parse_records([Record2 | Parsed], Info, Else).

parse_header(P, Opts, Header) ->
  parse_header(P, Opts, Header, 0).

parse_header(P, 0, <<>>, _N) ->
  P;
parse_header(P, OptFlag, Header, N) when (OptFlag band 1) =:= 0 ->
  parse_header(P, OptFlag bsr 1, Header, N + 1);
parse_header(P, OptFlag, <<DataBin:4/binary, Header/binary>>, N) ->
  {Key, Val} = parse_header_field(N, DataBin),
  parse_header(maps:put(Key, Val, P), OptFlag bsr 1, Header, N + 1).

parse_header_field(2, <<Sec:?UINT>>) ->
  {eventtime, calendar:gregorian_seconds_to_datetime(63429523200 + Sec)};
parse_header_field(1, <<EventID:?UINT>>) -> {event_id, EventID};
parse_header_field(0, <<ObjectID:?UINT>>) -> {object_id, ObjectID}.

parse_subrecord(P, _, response, Data) ->
  parse_response(P, Data);
parse_subrecord(P, Service, SubRecord, Data) ->
  M = module(Service),
  '_debug'("calling ~w:~w", [M, SubRecord]),
  M:SubRecord(P, Data).

parse_response(P, <<MsgId:?USHORT, Status:?BYTE>>) ->
  P#{r => #{msg_id => MsgId, status => Status}}.

service( 0)  -> response;
service( 1)  -> auth;
service( 2)  -> teledata;
service( 3)  -> commands;
service( 9)  -> firmware;
service(10)  -> ecall;
service(response)  ->  0;
service(auth)      ->  1;
service(teledata)  ->  2;
service(commands)  ->  3;
service(firmware)  ->  9;
service(ecall)     -> 10.

subrecord(_, 0)             -> response;
subrecord(auth, 1)          -> term_identify;
subrecord(auth, 5)          -> dispatcher_identity;
subrecord(auth, 6)          -> params;
subrecord(auth, 7)          -> info;
subrecord(auth, 8)          -> service_info;
subrecord(auth, 9)          -> result;


subrecord(teledata, 16)     -> position;
subrecord(teledata, 17)     -> position_ext;
subrecord(teledata, 18)     -> sensors;
subrecord(teledata, 19)     -> counters;
subrecord(teledata, 20)     -> state;
subrecord(teledata, 21)     -> accel;
subrecord(teledata, 22)     -> loopin;
subrecord(teledata, 23)     -> abs_digital;
subrecord(teledata, 24)     -> abs_analog;
subrecord(teledata, 25)     -> abs_counter;
subrecord(teledata, 26)     -> abs_loopin;
subrecord(teledata, 27)     -> lls;
subrecord(teledata, 28)     -> passengers_counters;
subrecord(teledata, 29)     -> can;
subrecord(teledata, 30)     -> identity;

subrecord(teledata, 15)     -> ext_santel_protobuf;

subrecord(teledata, _N)     ->
  '_warning'("skipping ~p", [_N]),
  skip.

module(auth) -> egts_auth;
module(teledata) -> egts_teledata;
module(commands) -> egts_commands;
module(firmware) -> egts_firmware;
module(ecall)    -> egts_ecall.
