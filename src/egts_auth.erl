-module(egts_auth).

-export([
  term_identify/1
  ]).

-include("egts_binary_types.hrl").
-include_lib("logger/include/log.hrl").

term_identify(<<TID:?UINT, Opts:?BYTE, Else/binary>>) ->
  trace("term identify opts ~w, data ~s", [Opts, Else]),
  Terms = term_identify(Opts, Else, 0),
  [{auth, [{terminal_id, TID} | Terms]}].

term_identify(0, _, _) ->
  [];
term_identify(Opts, Data, N) when (Opts band 1 =/= 1) ->
  term_identify(Opts bsr 1, Data, N + 1);
term_identify(Opts, <<Val:?USHORT, Else/binary>>, N)
    when N =:= 0 ->
  [{hdid, Val}] ++ term_identify(Opts bsr 1, Else, N + 1);
term_identify(Opts, <<Val:15/binary, Else/binary>>, N)
    when N =:= 1 ->
  [{imei, binary_to_integer(Val)}] ++ term_identify(Opts bsr 1, Else, N + 1);
term_identify(Opts, <<Val:16/binary, Else/binary>>, N)
    when N =:= 2 ->
  [{imsi, Val}] ++ term_identify(Opts bsr 1, Else, N + 1);
term_identify(Opts, <<Val:3/binary, Else/binary>>, N)
    when N =:= 3 ->
  [{lngc, Val}] ++ term_identify(Opts bsr 1, Else, N + 1);
term_identify(Opts, Else, N)
    when N =:= 4 ->
  [{ssra, 1}] ++ term_identify(Opts bsr 1, Else, N + 1);
term_identify(Opts, <<_:4, MCC:10, MNC:10, Else/binary>>, N)
    when N =:= 5 ->
  [{mcc, MCC} + {mnc, MNC}] ++ term_identify(Opts bsr 1, Else, N + 1);
term_identify(Opts, <<Val:?USHORT, Else/binary>>, N)
    when N =:= 6 ->
  [{bs, Val}] ++ term_identify(Opts bsr 1, Else, N + 1);
term_identify(Opts, <<Val:15/binary, Else/binary>>, N)
    when N =:= 7 ->
  [{msisdn, Val}] ++ term_identify(Opts bsr 1, Else, N + 1).
