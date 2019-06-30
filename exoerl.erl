-module(exoerl).
-export([go/2, escape/1, unescape/1, xor_csum/1]).

go(Host, What) when is_list(What) ->
  % io:format("What: ~p: ~n", [What]),
  timer:sleep(222),
  {ok, Sock} = gen_tcp:connect(Host, 26486, [binary, {packet, 0}]),
  Vals = [get_data(Sock, W) || W <- What ],
  % io:format("~p~n", [Val]);
  ok = gen_tcp:close(Sock),
  lists:zip(What,Vals);
go(Host, What) ->
  go(Host, [What]).

get_data(Sock, What0) ->
  What = temp(What0),
  % Pkt = mk_tx_packet(<<16#ff, 16#1e, 16#c8, 16#04, 16#34, 16#03, 16#08, 16#09>>),
  Pkt = mk_tx_packet(<<16#ff, 16#1e, 16#c8, 16#04, 16#b6, 16#03, What/binary>>),
  ok = gen_tcp:send(Sock, Pkt),
  Res = do_recv(Sock),
  % io:format("Res: ~p~n", [Res]),
  Val = decode_message(Res),
  Val.

% every third "address" appears to be a float
temp(outdoor) -> <<16#02, 16#0c>>;
temp(radiator) -> <<16#02, 16#0f>>;
temp(hot_water) -> <<16#02, 16#1b>>;
temp(district_heating) -> <<16#02, 16#54>>;
temp(hot_water_regulator) -> <<16#04, 16#e4>>;
temp(Other) when is_binary(Other) -> Other;
temp(Other) -> <<16#02, Other>>.

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

decode_message(Bin0) ->
  Bin = unescape(Bin0),
  PayloadLen = byte_size(Bin)-3,
  <<16#3d,Payload:PayloadLen/binary, Csum, 16#3e>> = Bin,
  case xor_csum(Payload) of
    Csum ->
      <<MsgLength,Msg/binary>> = Payload,
      case MsgLength =:= byte_size(Msg) of
        true ->
          value(Msg);
        false ->
          {error, bad_length}
      end;
    _ ->
      {error, bad_checksum}
  end.

% EXOFLOAT - 0x00 ++ float32
value(<<16#00,16#ffffffff:32>>) -> 'NaN';
value(<<16#00,V:32/float-little>>) -> V;
% EXOSHORT - one byte
value(<<Short>>) -> status(Short);
value(_) -> {error, unknown_data}.

status(0) -> stopped;
status(S) when (S > 0) and (S < 5) -> starting;
status(5) -> running;
status(11) -> stopping;
status(12) -> fire_alarm;
status(_) -> undefined.
