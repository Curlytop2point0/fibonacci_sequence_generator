-module(fibonacci).
-export([fib/1, print_fib_sequence/1, start/0, stop/0, fib_acc/3]).

% Recursive function to calculate the nth Fibonacci number
fib(N) when N >= 0 -> fib_acc(N, 0, 1).

% Helper function using tail recursion for efficiency
fib_acc(0, A, _) -> A;
fib_acc(N, A, B) when N > 0 -> fib_acc(N - 1, B, A + B).

% Function to print the Fibonacci sequence up to the nth number
print_fib_sequence(N) when N >= 0 ->
    Seq = lists:map(fun fib/1, lists:seq(0, N)),
    io:format("Fibonacci sequence up to ~p: ~p~n", [N, Seq]).

% Start function to create a process and register it if not already registered
start() ->
    case whereis(fib_proc) of
        undefined ->
            Pid = spawn(fun() -> loop() end),
            register(fib_proc, Pid),
            Pid;
        Pid ->
            Pid
    end.

% Stop function to terminate the registered process
stop() ->
    Pid = whereis(fib_proc),
    if
        Pid =/= undefined -> exit(Pid, normal);
        true -> ok
    end.

% Loop function to handle messages
loop() ->
    receive
        {calculate, N} when is_integer(N) ->
            Result = fib(N),
            io:format("Fibonacci number ~p: ~p~n", [N, Result]),
            loop();
        stop ->
            io:format("Stopping process~n"),
            ok
    end.
