-module(egts_auth).

-export([response/1]).
-export([term_identify/2]).
-export([dispatcher_identity/2]).

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
