-module(egts_auth).

-export([response/1]).
-export([term_identify/2]).
-export([dispatcher_identity/2]).
-export([params/2]).
-export([info/2]).
-export([service_info/2]).

-include("egts_binary_types.hrl").
-include_lib("logger/include/log.hrl").

response(Data) -> egts_service:parse(response, Data).

term_identify(P, <<TID:?UINT, Opts:?BYTE, Else/binary>>) ->
  '_trace'("term identify opts ~w, data ~s", [Opts, Else]),
  term_identify(P#{type => authentication, terminal_id => TID}, Opts, Else, 0).

term_identify(P, 0, _, _) ->
  '_debug'("identify: ~w", [P]),
  P;
term_identify(P, Opts, Data, N) when (Opts band 1 =/= 1) ->
  term_identify(P, Opts bsr 1, Data, N + 1);
term_identify(P, Opts, <<Val:?USHORT, Else/binary>>, N)
    when N =:= 0 ->
  term_identify(P#{hdid => Val}, Opts bsr 1, Else, N + 1);
term_identify(P, Opts, <<Val:15/binary, Else/binary>>, N)
    when N =:= 1 ->
  term_identify(P#{imei => binary_to_integer(Val)}, Opts bsr 1, Else, N + 1);
term_identify(P, Opts, <<Val:16/binary, Else/binary>>, N)
    when N =:= 2 ->
  term_identify(P#{imsi => Val}, Opts bsr 1, Else, N + 1);
term_identify(P, Opts, <<Val:3/binary, Else/binary>>, N)
    when N =:= 3 ->
  term_identify(P#{lngc => Val}, Opts bsr 1, Else, N + 1);
term_identify(P, Opts, Else, N)
    when N =:= 4 ->
  term_identify(P#{ssra => 1}, Opts bsr 1, Else, N + 1);
term_identify(P, Opts, <<_:4, MCC:10, MNC:10, Else/binary>>, N)
    when N =:= 5 ->
  term_identify(P#{mcc => MCC, mnc => MNC}, Opts bsr 1, Else, N + 1);
term_identify(P, Opts, <<Val:?USHORT, Else/binary>>, N)
    when N =:= 6 ->
  term_identify(P#{bs => Val}, Opts bsr 1, Else, N + 1);
term_identify(P, Opts, <<Val:15/binary, Else/binary>>, N)
    when N =:= 7 ->
  term_identify(P#{msisdn => Val}, Opts bsr 1, Else, N + 1).

dispatcher_identity(P, <<DT:?BYTE, DID:?UINT, DSCR/binary>>) ->
  case DSCR of
    <<>> -> P#{dispatcher => #{type => DT, id => DID}};
    DSCR -> P#{dispatcher => #{type => DT, id => DID, description => DSCR}}
  end.

params(P, <<FLGBits:5, PKE:1, ENA:2, PKL:?USHORT, PBK:PKL/binary, Rest/binary>>)
  when ENA =/= 0 andalso PKE =:= 1 ->
  params(misc:update_path([auth, public_key], PBK, P), <<FLGBits:5, 0:3, Rest/binary>>);
params(P, <<FLGBits:4, 1:1, 0:3, ISL:?USHORT, Rest/binary>>) ->
  params(misc:update_path([auth, isl], ISL, P), <<FLGBits:4, 0:4, Rest/binary>>);
params(P, <<FLGBits:3, 1:1, 0:4, MSZ:?USHORT, Rest/binary>>) ->
  params(misc:update_apth([auth, msz], MSZ, P), <<FLGBits:3, 0:5, Rest/binary>>);
params(P, <<FLGBits:2, 1:1, 0:5, Data/binary>>) ->
  [SS, Rest] = binary:split(Data, <<0>>),
  params(misc:update_path([auth, server_sequence], SS, P), <<FLGBits:2, 0:6, Rest/binary>>);
params(P, <<_:1, 1:1, 0:6, Data/binary>>) ->
  [EXP, Rest] = binary:split(Data, <<0>>),
  params(misc:update_path([auth, exp], EXP, P), <<0:8, Rest/binary>>);
params(P, <<_:1, 0:7, _Unparsed/binary>>) ->
  '_warning'(_Unparsed =/= <<>>, "unparsed ~p", [_Unparsed]),
  P.

info(P, Data) ->
  [UNM, UPSW | Rest] = binary:split(Data, <<0>>, [global]),
  P1 = misc:update_path([auth, user], UNM, P),
  P2 = misc:update_path([auth, password], UPSW, P1),
  case Rest of
    [<<>>] ->
      P2;
    [SS, <<>>] ->
      misc:update_path([auth, server_sequence], SS, P2)
  end.

service_info(P, <<ST:?BYTE, SST:?BYTE, SRVA:1, _:5, SRVRP:2>>) ->
  Type = egts_service:service(ST),
  State = case SST of
            0 -> in_service;
            128 -> out_of_service;
            129 -> denied;
            130 -> no_conf;
            131 -> temp_unavail
          end,
  ReqType = case SRVA of
              0 -> supported;
              1 -> request
              end,
  Priority = case SRVRP of
               0 -> highest;
               1 -> high;
               2 -> middle;
               3 -> low
             end,
  misc:update_path([services, ReqType, Type], #{state => State, priority => Priority}, P).
