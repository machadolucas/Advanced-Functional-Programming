% Write an Erlang program that spawns two processes and gives them both a list of integers. Then the processes
% start taking turns, sending each other at one time one integer from the list. If the receiver receives an
% integer that is in its original list of integers, it sends information about that to the sender.
% Then both will terminate.
-module(t74).
-export([start/0, ping/3, pong/1]).

ping(_List, 0, Pong_PID) ->
    Pong_PID ! finished,
    io:format("Ping finished~n", []);

ping(List, N, Pong_PID) ->
    Element = lists:nth(N, List),
    Pong_PID ! {ping, Element, N, self()},
    receive
        finished ->
            io:format("Ping finished~n", []);
        {existing, Message} ->
            io:format("Pong said that ~p in its list. Finalizing ping~n", [Message]);
        {pong, Message} ->
            io:format("Ping received from pong: ~p~n", [Message]),
            case lists:member(Message, List) of
                true ->  Pong_PID ! {existing, Message},
                         io:format("~p is in ping list. Notifying pong and finalizing ping~n", [Message]);
                false -> ok
            end
    end,
    ping(List, N - 1, Pong_PID).

pong(List) ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {existing, Message} ->
            io:format("Ping said it has element in its list: ~p~n", [Message]),
            io:format("Pong finalizing~n", []);
        {ping, Message, Index, Ping_PID} ->
            io:format("Pong received from ping: ~p~n", [Message]),
            case lists:member(Message, List) of
                true ->  Ping_PID ! {existing, Message},
                         io:format("~p is in pong list. Notifying ping and finalizing pong~n", [Message]);
                false -> Element = lists:nth(Index, List),
                         Ping_PID ! {pong, Element},
                         pong(List)
            end
    end.

start() ->
    Pong_PID = spawn(t74, pong, [[3,3,5,7,9,11]]),
    spawn(t74, ping, [[2,4,6,7,8,10], 6, Pong_PID]).
