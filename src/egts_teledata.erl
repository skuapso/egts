-module(egts_teledata).

-export([position/2]).
-export([position_ext/2]).
-export([sensors/2]).
-export([counters/2]).
-export([state/2]).
-export([loopin/2]).
-export([abs_digital/2]).
-export([abs_analog/2]).
-export([abs_counter/2]).
-export([abs_loopin/2]).
-export([lls/2]).
-export([passengers_counters/2]).
-export([accel/2]).
-export([can/2]).
-export([identity/2]).
-export([response/1]).

-include("egts_binary_types.hrl").
-include_lib("logger/include/log.hrl").

response(Data) -> egts_service:parse(response, Data).

position(P, <<
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

  P1 = P#{
    eventtime => calendar:gregorian_seconds_to_datetime(63429523200 + NTM),
    location => #{
      course => Dir + 256 * DIRH,
      latitude => sign(LAHS)*Lat*90/16#ffffffff,
      longitude=> sign(LOHS)*Lon*180/16#ffffffff},
    parking => MV bxor 1,
    offline => BB,
    fix => FIX,
    cs => CS,
    valid => VLD,
    speed => SPD/10,
    odometer => ODM,
    event => SRC},
  P2 = position_opt_data(P1, ALTE, ALTS, SRC, Else),
  '_debug'("navigation data: ~w", [P2]),
  misc:update_path(
    [digital, in],
    misc:bits2map(8, DIn),
    P2).

position_opt_data(P, 1, Sign, SRC, <<Alt:?UINT3, Rest/binary>>) ->
  src_data(
    misc:update_path(
      [location, altitude],
      sign(Sign) * Alt,
      P),
    SRC,
    Rest);
position_opt_data(P, 0, _, SRC, Data) ->
  src_data(P, SRC, Data).

src_data(P, _, <<>>) -> P;
src_data(P, _SRC, <<SRCD:?SHORT>>) -> P#{src_data => SRCD}.

position_ext(P, <<_:3, Bits:5, Rest/binary>>) ->
  position_ext(P, Bits, Rest, 1).
position_ext(P, 0, <<>>, _N) ->
  P;
position_ext(P, 0, Data, _N) ->
  '_warning'("unparsed data ~s", [Data]),
  P;
position_ext(P, Bits, <<Val:?USHORT, Rest/binary>>, 1 = N) when Bits band 1 =:= 1 ->
  position_ext(P#{vdop => Val}, Bits bsr 1, Rest, N + 1);
position_ext(P, Bits, <<Val:?USHORT, Rest/binary>>, 2 = N) when Bits band 1 =:= 1 ->
  position_ext(P#{hdop => Val} ,Bits bsr 1, Rest, N + 1);
position_ext(P, Bits, <<Val:?USHORT, Rest/binary>>, 3 = N) when Bits band 1 =:= 1 ->
  position_ext(P#{pdop => Val}, Bits bsr 1, Rest, N + 1);
position_ext(P, Bits, <<Val:?BYTE, Rest/binary>>, 4 = N) when Bits band 1 =:= 1 ->
  position_ext(P#{used => Val}, Bits bsr 1, Rest, N + 1);
position_ext(P, Bits, <<Val:?USHORT, Rest/binary>>, 5 = N) when Bits band 1 =:= 1 ->
  position_ext(P#{navigation_systems => nav_systems(Val)}, Bits bsr 1, Rest, N + 1);
position_ext(P, Bits, BinData, N) ->
  position_ext(P, Bits bsr 1, BinData, N + 1).

sensors(P, <<DInSensorsBl:8, DOutSensors:8, ASensors:8, Else/binary>>) ->
  P1 = case misc:bits2map(8, DOutSensors) of
           #{} -> P;
           A  -> misc:update_path([digital, out], A, P)
         end,
  {P2, Bin1} = digital_in(P1, DInSensorsBl, Else),
  analog(P2, ASensors, Bin1).

counters(P, <<B:8, Else/binary>>) -> counters(P, B, Else, 1).
counters(P, 0, <<>>, _N) ->
  P;
counters(P, I, Data, N) when I band 1 =/= 1 ->
  counters(P, I bsr 1, Data, N + 1);
counters(P, I, <<C:?UINT3, Else/binary>>, N) ->
  counters(
    misc:update_path(
      [counter, N],
      C,
      P),
    I bsr 1,
    Else,
    N + 1).

state(P, <<ST:?BYTE, MPSV:?BYTE, BBV:?BYTE, IBV:?BYTE, _:5, NMS:1, BBU:1, IBU:1>>) ->
  P#{
    state => state(ST),
    using_navigation => NMS,
    using_backup_battery => BBU,
    using_internal_battery => IBU,
    external_power => MPSV/10,
    backup_battery => BBV/10,
    internal_battery => IBV/10}.
state(0)  -> passive;
state(1)  -> era;
state(2)  -> active;
state(3)  -> extra_call;
state(4)  -> extra_tracking;
state(5)  -> testing;
state(6)  -> service;
state(7)  -> firmware.

loopin(P, <<I:8, Data/binary>>) -> loopin(P, Data, I, 0).
loopin(P, BinData, 0, _N) when (BinData =:= <<>>) or (BinData =:= <<0:4>>) ->
  P;
loopin(P, Unparsed, 0, N) ->
  '_warning'("unparsed data in loopin ~w, N ~w", [Unparsed, N]),
  loopin(P, <<>>, 0, N);
loopin(P, <<LIS:4, Else/binary>> = BinData, I, N) when I band 1 =:= 1 ->
  '_trace'("parsing ~w", [BinData]),
  loopin(
    misc:update_path(
      [loop, in, N],
      LIS,
      P),
    Else,
    I bsr 1,
    N + 1);
loopin(P, BinData, I, N) ->
  '_trace'("parsing ~w", [BinData]),
  loopin(P, BinData, I bsr 1, N + 1).

abs_digital(P, <<Low:4, Val:4, High:8>>) ->
  <<N:?USHORT>> = <<0:4, Low:4, High:8>>,
  misc:update_path(
    [digital, in, N],
    if Val =:= 0 -> 0; true -> 1 end,
    P).

abs_analog(P, <<N:?BYTE, Val:?FLOAT>>) ->
  misc:update_path(
    [analog, in, N],
    Val/100,
    P).

abs_counter(P, <<N:?BYTE, Val:?UINT3>>) ->
  misc:update_path(
    [counter, N],
    Val,
    P).

abs_loopin(P, <<Low:4, Val:4, High:8>>) ->
  <<N:?USHORT>> = <<0:4, Low:4, High:8>>,
  misc:update_path(
    [loop, in, N],
    Val,
    P).

lls(P, <<>>) -> P;
lls(P, <<_:1, 1:1, _:2, _:1, N:3, Port:?USHORT, _/binary>>) ->
  '_warning'("lls reading error"),
  LLSs = maps:get(lls, P, []),
  misc:update_path(
    [lls],
    [#{port => N, mac => Port, units => error} | LLSs],
    P);
lls(P, <<_:1, 0:1, 2#00:2, 0:1, N:3, Port:?USHORT, Val:?UINT>>) ->
  LLSs = maps:get(lls, P, []),
  misc:update_path(
    [lls],
    [#{port => N, mac => Port, val => Val, units => none} | LLSs],
    P);
lls(P, <<_:1, 0:1, 2#01:2, 0:1, N:3, Port:?USHORT, Val:?UINT>>) ->
  LLSs = maps:get(lls, P, []),
  misc:update_path(
    [lls],
    [#{port => N, mac => Port, val => Val, units => percent} | LLSs],
    P);
lls(P, <<_:1, 0:1, 2#10:2, 0:1, N:3, Port:?USHORT, Val:?UINT>>) ->
  LLSs = maps:get(lls, P, []),
  misc:update_path(
    [lls],
    [#{port => N, mac => Port, val => Val/10, units => liter} | LLSs],
    P);
lls(P, Data) -> '_warning'("unsupported lls data ~s", [Data]), P.

passengers_counters(P, <<>>) -> P;
passengers_counters(P, <<_:7, 1:1, DPR:?BYTE, DRL:?BYTE, Port:?USHORT, Rest/binary>>) ->
  misc:update_path(
    [passenges_bin, Port],
    #{doors_presented => DPR,
      doors_released => DRL,
      data => Rest},
    P);
passengers_counters(P, <<_:7, 0:1, DPR:?BYTE, DRL:?BYTE, Port:?USHORT, Rest/binary>>) ->
  P1 = misc:update_path(
         [passengers, Port],
         #{doors_presented => DPR,
           doors_released => DRL},
         P),
  passengers_counters(P1, Port, DPR, Rest);
passengers_counters(P, _Data) -> '_warning'("not parsed passengers counter"), P.

passengers_counters(P, _Port, 0, <<>>) -> P;
passengers_counters(P, Port, Bits, <<In:?BYTE, Out:?BYTE, Rest/binary>>)
    when Bits band 1 =:= 1 ->
  passengers_counters(
    misc:update_path(
      [passengers, Port, in],
      In,
      misc:update_path(
        [passengers, Port, out],
        Out,
        P)),
    Port,
    Bits bsr 1,
    Rest);
passengers_counters(P, Port, Bits, Data) ->
  passengers_counters(P, Port, Bits bsr 1, Data).

accel(P, <<SA:?BYTE, ATM:?UINT, AccelData/binary>>) when byte_size(AccelData) =:= SA * 8 ->
  accel(P, ATM, AccelData);
accel(P, <<SA:?BYTE, _ATM:?UINT, AccelData/binary>>) ->
  '_warning'("accel format error: SA is ~p and data length is ~p", [SA, byte_size(AccelData)]),
  P.

accel(P, ATM, <<RTM:?USHORT, XAAV:?SHORT, YAAV:?SHORT, ZAAV:?SHORT, Rest/binary>>) ->
  Accel = maps:get(accel, P, []),
  accel(misc:update_path([accel],
                         [#{time => ATM + RTM,
                            x => XAAV,
                            y => YAAV,
                            z => ZAAV
                           } | Accel],
                         P),
       ATM,
       Rest);
accel(P, _ATM, <<>>) ->
  Accel = maps:get(accel, P),
  misc:update_path([accel], lists:reverse(Accel), P).

can(P, <<0:8, 0:8, Data/binary>>) ->
  parse_can(P, Data).

parse_can(P, <<0:?BYTE, 1:?BYTE, 4:?BYTE, CanMHS:?UINT, Rest/binary>>) ->
  parse_can(misc:update_path([can, mhs], CanMHS * 0.05, P), Rest);
parse_can(P, <<0:?BYTE, 2:?BYTE, 4:?BYTE, Dist:?UINT, Rest/binary>>) ->
  parse_can(misc:update_path([can, odometr], Dist * 0.125, P), Rest);
parse_can(P, <<0:?BYTE, 3:?BYTE, 1:?BYTE, Speed:?BYTE, Rest/binary>>) ->
  parse_can(misc:update_path([can, speed], Speed, P), Rest);
parse_can(P, <<1:?BYTE, 1:?BYTE, 4:?BYTE, FFC:?UINT, Rest/binary>>) ->
  parse_can(misc:update_path([can, full_fuel_consamption], FFC/2, P), Rest);
parse_can(P, <<2:?BYTE, 1:?BYTE, 1:?BYTE, Temp:?BYTE, Rest/binary>>) ->
  parse_can(misc:update_path([can, coolant_temp], Temp - 40, P), Rest);
parse_can(P, <<2:?BYTE, 2:?BYTE, 2:?BYTE, ETPM:?USHORT, Rest/binary>>) ->
  parse_can(misc:update_path([can, engine_turns_per_minute], ETPM * 0.125, P), Rest);
parse_can(P, <<2:?BYTE, 3:?BYTE, 1:?BYTE, Accel:?BYTE, Rest/binary>>) ->
  parse_can(misc:update_path([can, accelerator], Accel, P), Rest);
parse_can(P, <<2:?BYTE, 4:?BYTE, 1:?BYTE, EngineLoad:?BYTE, Rest/binary>>) ->
  parse_can(misc:update_path([can, engine_load], EngineLoad, P), Rest);
parse_can(P, <<3:?BYTE, Axis:?BYTE, 2:?BYTE, Weight:?USHORT, Rest/binary>>) ->
  parse_can(misc:update_path([can, axis_weight, Axis], Weight * 0.5, P), Rest);
parse_can(P, <<6:?BYTE, 0:?BYTE, 2:?BYTE, State:?USHORT, Rest/binary>>) ->
  parse_can(misc:update_path([can, digital, in], misc:bits2map(16, State), P), Rest);
parse_can(P, <<6:?BYTE, 3:?BYTE, 4:?BYTE, Square:?UINT, Rest/binary>>) ->
  parse_can(misc:update_path([can, square], Square * 0.01, P), Rest);
parse_can(P, <<Group:?BYTE, Type:?BYTE, Len:?BYTE, Data:Len/binary, Rest/binary>>) ->
  '_warning'("unparsed can data: group ~p, type ~p, data ~p", [Group, Type, Data]),
  parse_can(P, Rest);
parse_can(P, <<>>) ->
  P.

%% internal functions
sign(0) ->  1;
sign(1) -> -1.


digital_in(P, I, Data) -> digital_in(P, I, Data, 0).
digital_in(P, 0, Data, _) -> {P, Data};
digital_in(P, I, <<DIn:8, Else/binary>>, N) when I band 1 =:= 1 ->
  digital_in(
    misc:update_path(
      [digital, in],
      maps:merge(
        maps:get(in, maps:get(digital, P, #{}), #{}),
        misc:bits2map(8, DIn, N)),
      P),
    I bsr 1,
    Else,
    N + 8);
digital_in(P, I, Data, N) ->
  digital_in(P, I bsr 1, Data, N + 8).

analog(P, I, Data) -> analog(P, I, Data, 1).
analog(P, 0, _, _) -> P;
analog(P, I, <<Val:?FLOAT, Else/binary>>, N) when I band 1 =:= 1 ->
  analog(misc:update_path(
           [analog, in, N],
           Val / 100,
           P),
         I bsr 1,
         Else,
         N + 1);
analog(P, I, Data, N) ->
  analog(P, I bsr 1, Data, N + 1).

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

identity(P, <<DeviceAddress:?USHORT, CardNumberBin/binary>>) ->
  CardNumber = list_to_integer(misc:string_to_hex(lists:reverse(binary_to_list(CardNumberBin)))),
  misc:update_path([identity, DeviceAddress], CardNumber, P).
