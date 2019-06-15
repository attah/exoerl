-module(exoerl).
-export([go/2, escape/1, unescape/1, xor_csum/1]).

go(Host, What0) ->
  % io:format("What: ~p~n", [What0]),
  % timer:sleep(666),
  {ok, Sock} = gen_tcp:connect(Host, 26486, [binary, {packet, 0}]),
  What = temp(What0),
  Pkt = mk_tx_packet(<<16#ff, 16#1e, 16#c8, 16#04, 16#b6, 16#03, 16#02, What>>),
  ok = gen_tcp:send(Sock, Pkt),
  Res = do_recv(Sock),
  ok = gen_tcp:close(Sock),
  io:format("Res: ~p~n", [Res]),
  Val = decode_value(Res),
  io:format("~p~n", [Val]).

% every third "address" appears to be a sensor
temp(outdoor) -> 16#0c;
temp(radiator) -> 16#0f;
temp(hot_water) -> 16#1b;
temp(district_heating) -> 16#54;
temp(Other) -> Other.

do_recv(_Sock) ->
  receive
    {tcp, _, Bs} -> Bs
  end.

mk_tx_packet(Bin) ->
  Csum = xor_csum(Bin),
  Escaped = escape(<<Bin/binary, Csum>>),
  <<16#3c, Escaped/binary, 16#3e>>.

xor_csum(Bin) ->
  lists:foldl(fun(B, Acc) -> B bxor Acc end, 0, binary_to_list(Bin)).

escape(Bin) ->
  Escaped =
    lists:foldl(fun(B, Acc) when (B == 16#1b)
                              or (B == 16#3c)
                              or (B == 16#3d)
                              or (B == 16#3e) -> Acc ++ [16#1b, B bxor 16#ff];
                   (B, Acc) -> Acc ++ [B] end,
                [],
                binary_to_list(Bin)),
  list_to_binary(Escaped).

unescape(Bin) ->
  {false, Unescaped} =
    lists:foldl(fun(16#1b, {false, Acc}) -> {true, Acc};
                   (B, {true, Acc}) -> {false, Acc++[B bxor 16#ff]};
                   (B, {false, Acc}) -> {false, Acc++[B]} end,
                {false, []},
                binary_to_list(Bin)),
  list_to_binary(Unescaped).

decode_value(Bin0) ->
  Bin = unescape(Bin0),
  PayloadLen = byte_size(Bin)-3,
  <<16#3d,Payload:PayloadLen/binary, Csum, 16#3e>> = Bin,
  case xor_csum(Payload) of
    Csum ->
      <<Status:8,Type:8,Msg/binary>> = Payload,
      case Type of
        0 -> % EXOFLOAT
          Val =
            case Msg of
              <<16#ff,16#ff,16#ff,16#ff>> ->
                'NaN';
              <<V:32/float-little>> ->
                V
            end,
          {status(Status), Val};
        1 -> % EXOSHORT - only status??
          status(Status)
      end;
    _ ->
      {error, bad_checksum}
  end.

status(0) -> stopped;
status(S) when (S > 0) and (S < 5) -> starting;
status(5) -> running;
status(11) -> stopping;
status(12) -> fire_alarm;
status(_) -> undefined.
