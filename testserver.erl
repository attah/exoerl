-module(testserver).
-export([start/0]).

start() ->
  {ok, LSock} = gen_tcp:listen(26486, [binary, {packet, 0},
                                      {active, false}]),
  {ok, Sock} = gen_tcp:accept(LSock),
  recv_loop(Sock),
  ok = gen_tcp:close(Sock),
  ok = gen_tcp:close(LSock).

recv_loop(Sock) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, <<16#3c,Data0/binary>>} ->
      Data = exoerl:unescape(Data0),
      PayloadLen = byte_size(Data)-2,
      <<Payload:PayloadLen/binary,Csum,16#3e>> = Data,
      case exoerl:xor_csum(Payload) of
        Csum ->
          ok = gen_tcp:send(Sock, reply(Payload)),
          % recv_loop(Sock); %keep serving
          ok;
        _ ->
          {error, bad_checksum}
      end;
    {ok, D} ->
      {error, {bad_data, D}};
    {error, closed} ->
      {error, closed}
  end.

reply(<<16#ff, 16#1e, 16#c8, 16#04, 16#b6, 16#03, 16#02, _>>) ->
    mk_rx_packet(<<16#05, 16#00, 21.8:32/float-little>>);
reply(<<_Stuff/binary>>) ->
    mk_rx_packet(<<16#01, 16#01>>).

mk_rx_packet(Bin) ->
  Csum = exoerl:xor_csum(Bin),
  Payload = exoerl:escape(<<Bin/binary, Csum>>),
  <<16#3d, Payload/binary, 16#3e>>.
