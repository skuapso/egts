-module(egts_teledata).

-export([position/1]).
-export([position_ext/1]).
-export([sensors/1]).
-export([counters/1]).
-export([state/1]).
-export([loopin/1]).
-export([abs_digital/1]).
-export([abs_analog/1]).
-export([abs_counter/1]).
-export([abs_loopin/1]).
-export([lls/1]).
-export([passengers_counters/1]).
-export([response/1]).

-include("egts_binary_types.hrl").
-include_lib("logger/include/log.hrl").

response(Data) -> egts_service:parse(response, Data).

position(<<
        NTM:?UINT,
        Lat:?UINT,
        Lon:?UINT,
        ALTE:1, LOHS:1, LAHS:1, MV:1, BB:1, CS:1, FIX:1, VLD:1,
        SPDLBits:8,
        DIRH:1, ALTS:1, SPDHBits:6,
        Dir:?BYTE,
        ODM:?UINT3,
        DIn:?BYTE,
        SRC:?BYTE,
        Else/binary>>) ->
  <<SPD:14>> = <<SPDHBits:6, SPDLBits:8>>,

  Navigation = [
      {eventtime, calendar:gregorian_seconds_to_datetime(63429523200 + NTM)},
      {latitude,  sign(LAHS)*Lat*90/16#ffffffff},
      {longitude, sign(LOHS)*Lon*180/16#ffffffff},
      {parking, MV bxor 1},
      {offline, BB},
      {fix, FIX},
      {cs, CS},
      {valid, VLD},
      {speed, SPD/10},
      {course, Dir + 256 * DIRH},
      {odometer, ODM},
      {event, SRC}
      ],
  OptData = position_opt_data(ALTE, ALTS, SRC, Else),
  debug("navigation data: ~w", [Navigation]),
  Set = case misc:bits2tuples(DIn) of
    [] -> [];
    DigitalIn -> [{set, [{digital_in, DigitalIn}]}]
  end,
  [{navigation, Navigation ++ OptData} | Set].

position_opt_data(1, Sign, SRC, <<Alt:?UINT3, Rest/binary>>) ->
  [{altitude, sign(Sign) * Alt} | src_data(SRC, Rest)];
position_opt_data(0, _, SRC, Data) ->
  src_data(SRC, Data).

src_data(_, <<>>) -> [];
src_data(_SRC, <<SRCD:?SHORT>>) -> [{src_data, SRCD}].

position_ext(<<_:3, Bits:5, Rest/binary>>) ->
  [{navigation_extra, position_ext(Bits, Rest, [], 1)}].
position_ext(0, <<>>, Parsed, _N) ->
  lists:reverse(Parsed);
position_ext(0, Data, Parsed, _N) ->
  warning("unparsed data ~s", [Data]),
  lists:reverse(Parsed);
position_ext(Bits, <<Val:?USHORT, Rest/binary>>, Parsed, 1 = N) when Bits band 1 =:= 1 ->
  H = {vdop, Val},
  position_ext(Bits bsr 1, Rest, [H | Parsed], N + 1);
position_ext(Bits, <<Val:?USHORT, Rest/binary>>, Parsed, 2 = N) when Bits band 1 =:= 1 ->
  H = {hdop, Val},
  position_ext(Bits bsr 1, Rest, [H | Parsed], N + 1);
position_ext(Bits, <<Val:?USHORT, Rest/binary>>, Parsed, 3 = N) when Bits band 1 =:= 1 ->
  H = {pdop, Val},
  position_ext(Bits bsr 1, Rest, [H | Parsed], N + 1);
position_ext(Bits, <<Val:?BYTE, Rest/binary>>, Parsed, 4 = N) when Bits band 1 =:= 1 ->
  H = {used, Val},
  position_ext(Bits bsr 1, Rest, [H | Parsed], N + 1);
position_ext(Bits, <<Val:?USHORT, Rest/binary>>, Parsed, 5 = N) when Bits band 1 =:= 1 ->
%  H = {navigation_systems, nav_systems(Val)},
  H = {navigation_systems, Val},
  position_ext(Bits bsr 1, Rest, [H | Parsed], N + 1);
position_ext(Bits, BinData, Parsed, N) ->
  position_ext(Bits bsr 1, BinData, Parsed, N + 1).

sensors(<<DInSensorsBl:8, DOutSensors:8, ASensors:8, Else/binary>>) ->
  DOut = case misc:bits2tuples(DOutSensors) of
    [] -> [];
    A  -> [{digital_out, A}]
  end,
  debug("digital out ~w", [DOut]),
  {DIn, Bin1} = digital_in(DInSensorsBl, Else),
  debug("digital in ~w", [DIn]),
  Analog = analog(ASensors, Bin1),
  debug("analog ~w", [Analog]),
  [{set, DIn ++ DOut ++ Analog}].

counters(<<B:8, Else/binary>>) -> counters(B, Else, [], 1).
counters(0, <<>>, Parsed, _N) ->
  [{set, [{counter, lists:reverse(Parsed)}]}];
counters(I, Data, Parsed, N) when I band 1 =/= 1 ->
  counters(I bsr 1, Data, Parsed, N + 1);
counters(I, <<C:?UINT3, Else/binary>>, Parsed, N) ->
  counters(I bsr 1, Else, [{N, C} | Parsed], N + 1).

state(<<ST:?BYTE, MPSV:?BYTE, BBV:?BYTE, IBV:?BYTE, _:5, NMS:1, BBU:1, IBU:1>>) ->
  [
    {navigation_extra, [{state, state(ST)}]},
    {set, [
        {boolean_sensor,
         [{navigation, NMS}, {backup_battery_using, BBU}, {internal_battery_using, IBU}]},
        {float_sensor, [{main_power, MPSV/10}, {backup_battery, BBV/10}, {internal_battery, IBV/10}]}
    ]}
  ];
state(0)  -> passive;
state(1)  -> era;
state(2)  -> active;
state(3)  -> extra_call;
state(4)  -> extra_tracking;
state(5)  -> testing;
state(6)  -> service;
state(7)  -> firmware.

loopin(<<I:8, Data/binary>>) -> loopin(Data, I, [], 0).
loopin(BinData, 0, Data, _N) when (BinData =:= <<>>) or (BinData =:= <<0:4>>) ->
  case(Data) of
    [] -> [];
    Data -> [{set, [{loop_in, lists:reverse(Data)}]}]
  end;

loopin(Unparsed, 0, Data, N) ->
  warning("unparsed data in loopin ~w, N ~w", [Unparsed, N]),
  loopin(<<>>, 0, Data, N);
loopin(<<LIS:4, Else/binary>> = BinData, I, Data, N) when I band 1 =:= 1, LIS =/= 0 ->
  trace("parsing ~w", [BinData]),
  loopin(Else, I bsr 1, [{N, LIS} | Data], N + 1);
loopin(BinData, I, Data, N) ->
  trace("parsing ~w", [BinData]),
  loopin(BinData, I bsr 1, Data, N + 1).

abs_digital(<<_:4, 0:4, _:8>>) -> [];
abs_digital(<<Low:4, _:4, High:8>>) ->
  <<N:?USHORT>> = <<0:4, Low:4, High:8>>,
  [{set, {digital_in, {N, 1}}}].

abs_analog(<<_:?BYTE, 0:24>>) -> [];
abs_analog(<<N:?BYTE, Val:3/binary>>) ->
  [{set, {analog, {N, Val}}}].

abs_counter(<<_:?BYTE, 0:24>>) -> [];
abs_counter(<<N:?BYTE, Val:?UINT3>>) ->
  [{set, {counter, {N, Val}}}].

abs_loopin(<<_:4, 0:4, _:8>>) -> [];
abs_loopin(<<Low:4, Val:4, High:8>>) ->
  <<N:?USHORT>> = <<0:4, Low:4, High:8>>,
  [{set, {loop_in, {N, Val}}}].

lls(<<>>) -> [];
lls(<<_:1, 1:1, _/binary>>) ->
  warning("lls reading error"),
  [];
lls(<<_:1, 0:1, 0:2, 0:1, _N:3, _Port:?USHORT, 0:?UINT>>) -> [];
lls(<<_:1, 0:1, 0:2, 0:1, N:3, Port:?USHORT, Val:?UINT>>) ->
  [{set, [{lls, [{N, Val}]}, {lls_port, [{Port, Val}]}]}];
lls(Data) -> warning("unsupported lls data ~s", [Data]), [].

passengers_counters(<<>>) -> [];
passengers_counters(<<_:7, 1:1, DPR:?BYTE, DRL:?BYTE, Port:?USHORT, Rest/binary>>) ->
  [{passengers_bin, [{doors_present, DPR}, {doors_released, DRL}, {port, Port}, {data, Rest}]}];
passengers_counters(<<_:7, 0:1, DPR:?BYTE, DRL:?BYTE, Port:?USHORT, Rest/binary>>) ->
  Info = {passengers, [{doors_present, DPR}, {doors_release, DRL}, {port, Port}]},
  [Info, {set, passengers_counters(DPR, Rest, [], 1)}].
passengers_counters(0, <<>>, Parsed, _N) -> lists:reverse(Parsed);
passengers_counters(Bits, <<In:?BYTE, Out:?BYTE, Rest/binary>>, Parsed, N)
    when Bits band 1 =:= 1 ->
  passengers_counters(Bits bsr 1, Rest,
                      [{passengers_in, {N, In}}, {passengers_out, {N, Out}} | Parsed], N + 1);
passengers_counters(Bits, Data, Parsed, N) ->
  passengers_counters(Bits bsr 1, Data, Parsed, N + 1).

%% internal functions
sign(0) ->  1;
sign(1) -> -1.


digital_in(I, Data) -> digital_in(I, Data, 1, []).
digital_in(0, Data, _, []) ->
  {[], Data};
digital_in(0, Data, _, DIn) ->
  {{digital_in, DIn}, Data};
digital_in(I, <<DIn:8, Else/binary>>, N, DIn) when I band 1 =:= 1 ->
  digital_in(I bsr 1, Else, N + 8, DIn ++ misc:bits2tuples(DIn, N));
digital_in(I, Data, N, DIn) ->
  digital_in(I bsr 1, Data, N + 8, DIn).

analog(I, Data) -> analog(I, Data, 1, []).
analog(0, _, _, []) ->
  [];
analog(0, _, _, Analog) ->
  [{analog, lists:reverse(Analog)}];
analog(I, <<Data:3/binary, Else/binary>>, N, Analog) when I band 1 =:= 1 ->
  analog(I bsr 1, Else, N + 1, [{N, Data} | Analog]);
analog(I, Data, N, Analog) ->
  analog(I bsr 1, Data, N + 1, Analog).

nav_systems(0) -> [];
nav_systems(I) -> nav_systems(I, 1, []).
nav_systems(0, _N, NavSys) -> NavSys;
nav_systems(I, N=1, NavSys) when I band 1 =:= 1 -> nav_systems(I bsr 1, N + 1, [glonass | NavSys]);
nav_systems(I, N=2, NavSys) when I band 1 =:= 1 -> nav_systems(I bsr 1, N + 1, [gps | NavSys]);
nav_systems(I, N=3, NavSys) when I band 1 =:= 1 -> nav_systems(I bsr 1, N + 1, [galileo | NavSys]);
nav_systems(I, N=4, NavSys) when I band 1 =:= 1 -> nav_systems(I bsr 1, N + 1, [compass | NavSys]);
nav_systems(I, N=5, NavSys) when I band 1 =:= 1 -> nav_systems(I bsr 1, N + 1, [beidou | NavSys]);
nav_systems(I, N=6, NavSys) when I band 1 =:= 1 -> nav_systems(I bsr 1, N + 1, [doris | NavSys]);
nav_systems(I, N=7, NavSys) when I band 1 =:= 1 -> nav_systems(I bsr 1, N + 1, [irmss | NavSys]);
nav_systems(I, N=8, NavSys) when I band 1 =:= 1 -> nav_systems(I bsr 1, N + 1, [qzss | NavSys]);
nav_systems(I, N, NavSys) -> nav_systems(I bsr 1, N + 1, NavSys).
