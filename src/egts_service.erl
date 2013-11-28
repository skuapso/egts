-module(egts_service).

-export([
  parse/1,
  response/1
  ]).

-include_lib("logger/include/log.hrl").
-include("egts_binary_types.hrl").

response(Data) ->
  Info = proplists:get_value(info, Data),
  RL = 3,
  RN = proplists:get_value(msg_id, Info),
  SSOD = proplists:get_value(ssod, Info),
  RSOD = proplists:get_value(rsod, Info),
  GRP = proplists:get_value(group_record, Info),
  RPP = proplists:get_value(rpp, Info),
  Opts = 0,
  SST = service(proplists:get_value(sst, Info)),
  RST = service(proplists:get_value(rst, Info)),
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
    Record/binary>>.

parse(Data) ->
  {ok, parse(Data, [])}.

parse(<<>>, P) ->
  lists:reverse(P);
parse(<<RL:?USHORT, RN:?USHORT, SSOD:1, RSOD:1, GRP:1, RPP:2, Opts:3, _/binary>> = Data, Records) ->
  OptHL = header_length(Opts),
  debug("header length ~w", [OptHL]),
  debug("record length ~w", [RL]),
  <<_:5/binary, OptHeader:OptHL/binary, SST:8, RST:8, RecordsBin:RL/binary, Else/binary>> = Data,
  debug("header: ~w, opts: ~w", [OptHeader, Opts]),
  Info = [
      {msg_id, RN},
      {ssod, SSOD},
      {rsod, RSOD},
      {group_record, GRP},
      {sst, service(SST)},
      {rst, service(RST)},
      {rpp, RPP}]
      ++ parse_header(Opts, OptHeader),
  debug("info ~w", [Info]),
  debug("record ~w", [RecordsBin]),
  Record = parse_records(service(SST), RecordsBin),
  debug("record data ~w", [Record]),
  CombinedRecord = combine_record(Record),
  debug("combined record data ~w", [CombinedRecord]),
  parse(Else, [[{info, Info} | CombinedRecord] | Records]).

header_length(F) -> header_length(F, 0).
header_length(0, L) -> L * 4;
header_length(F, L) -> header_length(F bsr 1, F band 1 + L).

parse_records(SST, Data) -> parse_records(SST, Data, []).

parse_records(_SST, <<>>, Records) -> lists:reverse(Records);
parse_records(SST, <<Type:?BYTE, L:?USHORT, Data:L/binary, Else/binary>>, Records) ->
  SR = subrecord(SST, Type),
  case misc:compact_list(parse_subrecord(SST, SR, Data)) of
    [] ->
      trace("empty subrecord"),
      parse_records(SST, Else, Records);
    SRData -> 
      debug("subrecord ~w", [SRData]),
      parse_records(SST, Else, [SRData] ++ Records)
  end.

parse_header(Opts, Header) ->
  parse_header(Opts, Header, [], 0).

parse_header(0, <<>>, Data, _N) ->
  misc:compact_list(lists:reverse(Data));
parse_header(OptFlag, Header, Data, N) when (OptFlag band 1) =:= 0 ->
  parse_header(OptFlag bsr 1, Header, Data, N + 1);
parse_header(OptFlag, <<DataBin:4/binary, Header/binary>>, Data, N) ->
  parse_header(OptFlag bsr 1, Header, [parse_header_field(N, DataBin) | Data], N + 1).

parse_header_field(0, <<Sec:?UINT>>) -> {eventtime, Sec};
parse_header_field(1, <<EventID:?UINT>>) -> {event_id, EventID};
parse_header_field(2, <<ObjectID:?UINT>>) -> {object_id, ObjectID}.

parse_subrecord(Service, SubRecord, Data) ->
  M = list_to_atom("egts_" ++ atom_to_list(Service)),
  trace("calling ~w:~w(~s)", [M, SubRecord, Data]),
  M:SubRecord(Data).

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

subrecord(auth, 1)          -> term_identify;

subrecord(teledata, 16)     -> position;
subrecord(teledata, 17)     -> position_ext;
subrecord(teledata, 18)     -> sensors;
subrecord(teledata, 19)     -> counters;
subrecord(teledata, 20)     -> state;
subrecord(teledata, 21)     -> state;
subrecord(teledata, 22)     -> loopin;
subrecord(teledata, 23)     -> abs_digital;
subrecord(teledata, 24)     -> abs_analog;
subrecord(teledata, 25)     -> abs_counter;
subrecord(teledata, 26)     -> abs_loopin;
subrecord(teledata, 27)     -> lls;
subrecord(teledata, 28)     -> passengers_counters.

combine_record(Record) -> combine_record(Record, [], []).
combine_record([], Combined, []) -> lists:reverse(Combined);
combine_record([], Combined, SubRecord) ->
  combine_record([], [misc:compact_list(lists:reverse(SubRecord)) | Combined], []);
combine_record([[{navigation, Data} | T1] | T], Combined, []) ->
  combine_record(T, Combined, [{navigation, Data} | T1]);
combine_record([[{navigation, Data} | T1] | T], Combined, SubRecord) ->
  combine_record(T,
                 [misc:compact_list(lists:reverse(SubRecord)) | Combined],
                 [{navigation, Data} | T1]);
combine_record([[{navigation_extra, Data} | T1] | T], Combined, SubRecord) ->
  trace("adding extra navigation data"),
  SR1 = [{navigation, Data} | T1],
  debug("sr1 ~w", [SR1]),
  SR2 = SR1 ++ SubRecord,
  SR = [[{navigation, Data} | T1] | SubRecord],
  debug("sr data before ~w", [SubRecord]),
  debug("sr2 data after ~w", [SR2]),
  debug("sr data after  ~w", [SR]),
  combine_record(T, Combined, SR2);
combine_record([[H] | T], Combined, SubRecord) ->
  combine_record(T, Combined, [H | SubRecord]).
