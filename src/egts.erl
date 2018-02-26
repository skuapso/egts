%%%-------------------------------------------------------------------
%%% @author Ilya Ashchepkov
%%% @copyright 2014 NskAvd
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(egts).

-behaviour(terminal).
-behaviour(supervisor).
-behaviour(application).

-export([handle_data/3]).
-export([closed/2]).

-export([crc8/1]).
-export([crc16/1]).

%% Terminal callbacks
-export([init/2]).
-export([uin/2]).
-export([parse/2]).
-export([answer/1]).
-export([handle_hooks/4]).
-export([handle_info/2]).
%% Terminal optional callbacks
-export([to_binary/2]).
-export([send/2]).
-export([connect/3]).
-export([setopts/2]).
-export([recv/2]).
%% Supervisor callbacks
-export([init/1]).
-export([start/0]).
-export([start/2]).
-export([stop/1]).

-include("egts_binary_types.hrl").
-include_lib("logger/include/log.hrl").

crc8(Data) -> egts_crc:crc8(Data).
crc16(Data) -> egts_crc:crc16(Data).

state(State) -> terminal:state(State, ?MODULE).

set_state(State, IState) -> terminal:set(State, {module, ?MODULE}, IState).

handle_data(Pid, Socket, Data) ->
  Pid ! {?MODULE, Socket, Data}.

closed(Pid, Socket) ->
  Pid ! {{?MODULE, closed}, Socket}.

connect(Host, Port, Opts) ->
  egts_socket:connect(Host, Port, Opts).

send(Socket, Data) when is_port(Socket) andalso is_binary(Data) ->
  Socket ! Data;
send(Pid, Data) when is_pid(Pid) ->
  egts_socket:send(Pid, Data);
send(Socket, Data) when is_port(Socket) ->
  {ok, Pid} = egts_reg:pid(Socket),
  send(Pid, Data).

setopts(Pid, Opts) ->
  egts_socket:setopts(Pid, Opts).

recv(Pid, L) when is_pid(Pid) ->
  egts_socket:recv(Pid, L);
recv(Socket, L) when is_port(Socket) ->
  {ok, Pid} = egts_reg:pid(Socket),
  recv(Pid, L).

%%%===================================================================
%%% Terminal callbacks
%%%===================================================================

init(Opts, State) ->
  '_debug'("options ~p", [Opts]),
  {tcp, Socket} = terminal:socket(State),
  {ok, Pid} = egts_socket:accept(Socket),
  NewState = terminal:set(
               set_state(State, (parse_opts(Opts))#{handler => Pid}),
               socket,
               {?MODULE, Socket}),
  {ok, NewState}.

uin(#{type := Type, frame := FrameWithCrc}, State) when Type =/= egts_pt_routing ->
  L = case byte_size(FrameWithCrc) of
        0 -> 0;
        N when N > 2 -> N -2
      end,
  <<Frame:L/binary, _FrameCrc/binary>> = FrameWithCrc,
  {[Record | _], _} = {Data, Infos} = egts_service:parse(Type, Frame),
  IState = state(State),
  IState1 = IState#{data => Data, infos => Infos},
  {UIN, IState2} = case Record of
                     %% TODO: it probably will be better to create new protocol
                     #{dispatcher := _Dispatcher} ->
                       %% TODO: it might be better to update first packet
                       {skip, IState1#{infos => (Infos ++ [(hd(Infos))#{auth => #{status => 0}}])}};
                     Record ->
                       UIN_ = case Record of
                               #{imei := IMEI} ->
                                 IMEI;
                               #{terminal_id := TerminalId} ->
                                 TerminalId;
                               #{object_id := ObjectId} ->
                                 ObjectId
                             end,
                       case maps:get(auth, IState) of
                         'true' ->
                           {UIN_, IState1#{infos => (Infos ++ [(hd(Infos))#{auth => #{status => 0}}])}};
                         _ ->
                           {UIN_, IState1}
                       end
                   end,
  {ok, UIN, set_state(State, IState2)}.

to_binary(Type, #{header := Header, header_crc := HeadCrc, frame := Frame})
  when Type =:= raw;
       Type =:= answer ->
  {ok, <<Header/binary, HeadCrc:?BYTE, Frame/binary>>};
to_binary(Type, #{frame := Frame})
  when Type =:= raw;
       Type =:= answer ->
  {ok, Frame}.

parse(#{type := Type, frame := FrameWithCrc} = Data, State)
  when Type =/= egts_pt_routing ->
  L = case byte_size(FrameWithCrc) of
        0 -> 0;
        N when N > 2 -> N -2
      end,
  <<Frame:L/binary, _FrameCrc/binary>> = FrameWithCrc,
  IState = state(State),
  {Packets, Infos} = case IState of
                       #{data := Packets_, infos := Infos_} ->
                         {Packets_, Infos_};
                       _ ->
                         egts_service:parse(Type, Frame)
                     end,
  Handler = maps:get(handler, IState),
  NewIState = Data#{infos => Infos, handler => Handler},
  NewState = set_state(State, NewIState),
  {ok, Packets, NewState}.

answer(State) ->
  IState = state(State),
  Infos = maps:get(infos, IState),
  From = maps:get(from, IState),
  To = maps:get(to, IState),
  PId = maps:get(packet_id, IState),
  Handler = maps:get(handler, IState),

  %% It specified for EGTS_PT_RESPONSE to
  %% prepend PACKET ID and PROCESSING STATUS
  %% before SERVICE RECORDS.
  %% In our case packet was fully processed
  AnswerFrame = iolist_to_binary([<<PId:?USHORT, 0:?BYTE>>,
                                  [egts_service:response(Info) || Info <- Infos]]),
  AnswerCrc = crc16(AnswerFrame),
  Answer = maps:remove(
             header_crc,
             maps:remove(
               header,
               IState#{frame => <<AnswerFrame/binary, AnswerCrc:?USHORT>>,
                       packet_id => PId,
                       type => egts_pt_response,
                       from => To,
                       to => From})),
  NewState = set_state(State, #{handler => Handler}),
  {ok, Answer, NewState}.

handle_hooks(_HookName, _Data, _HooksReplies, _State) ->
  ok.

handle_info({{?MODULE, closed}, Socket}, State) ->
  NewState = case terminal:socket(State) of
               {?MODULE, Socket} ->
                 terminal:set(State, close, normal);
               _ -> State
             end,
  {ok, NewState};
handle_info({'EXIT', Handler, Reason} = Msg, State) ->
  case terminal:socket(State) of
    {?MODULE, Handler} ->
      {ok, terminal:set(State, close, Reason)};
    _ ->
      '_warning'("died unknown process ~p", [Msg]),
      {ok, State}
  end;
handle_info(Msg, State) ->
  '_warning'("unknown info msg ~w", [Msg]),
  {ok, State}.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(_Opts) ->
  {ok,
   {
    {one_for_one, 5, 10},
    [
     {
      egts_reg,
      {egts_reg, start_link, []},
      permanent,
      5000,
      worker,
      []
     }
    ]
   }
  }.

stop(_Stop) ->
  ok.

start() ->
  application:start(?MODULE).

start(_StartType, StartArgs) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).
%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_opts(Opts) ->
  parse_opts(Opts, #{}).

parse_opts([], Opts) -> Opts;
parse_opts([Auth | Rest], Opts) when Auth =:= auth orelse Auth =:= {auth, true} ->
  parse_opts(Rest, Opts#{auth => true});
parse_opts([_ | Rest], Opts) ->
  parse_opts(Rest, Opts).

%% vim: ft=erlang
