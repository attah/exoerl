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
    {ok, _B} ->
      ok = gen_tcp:send(Sock, mk_rx_packet(<<16#05, 16#00, 21.8:32/float-little>>)),
      % recv_loop(Sock); %keep serving
      ok;
    {error, closed} ->
      {error, closed}
  end.

mk_rx_packet(Bin) ->
  Csum = exoerl:xor_csum(Bin),
  Payload = exoerl:escape(<<Bin/binary, Csum>>),
  <<16#3d, Payload/binary, 16#3e>>.
