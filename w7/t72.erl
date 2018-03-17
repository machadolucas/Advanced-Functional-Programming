% Write an Erlang program that spawns a process and sends it a string. The process that receives the string adds
% a "hello, " in the front of that string and sends it back. Then the program prints it out.
-module(t72).
-export([start/1, greet/0]).


start(Name) ->
    Greet_PID = spawn(t72, greet, []),
    Greet_PID ! {Name, self()},
    receive
        {Result} ->
            io:fwrite(Result)
    end.


greet() ->
    receive
        {Name, SenderPid} ->
            Result = io:format("hello, ~p~n", [Name]),
            SenderPid ! {Result}
    end.

