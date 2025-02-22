-module(fibonacci).
-export([fib/1, print_fib_sequence/1, start/0, stop/0, fib_acc/3, validate_input/1, handle_error/1, sum_fib_sequence/1, print_sum_fib_sequence/1, reverse_fib_sequence/1, print_reverse_fib_sequence/1]).

% Function to validate input
% Makes sure the input is a non-negative integer
validate_input(N) when is_integer(N), N >= 0 ->
    true;
validate_input(_) ->
    false.

% Recursive function to calculate the nth Fibonacci number
% Uses an accumulator to optimize recursion
fib(N) ->
    case validate_input(N) of
        true -> fib_acc(N, 0, 1);
        false -> handle_error(invalid_input)
    end.

% Helper function using tail recursion for efficiency
% A and B are accumulators to store intermediate results
fib_acc(0, A, _) -> A;
fib_acc(N, A, B) when N > 0 -> fib_acc(N - 1, B, A + B).

% Function to handle errors
handle_error(invalid_input) ->
    io:format("Error: Invalid input. Please enter a non-negative integer.~n"),
    error(invalid_input);
handle_error(_) ->
    io:format("Error: Unknown error occurred.~n"),
    error(unknown_error).

% Function to print the Fibonacci sequence up to the nth number
% Validates input before generating the sequence
print_fib_sequence(N) ->
    case validate_input(N) of
        true ->
            Seq = lists:map(fun fib/1, lists:seq(0, N)),
            io:format("Fibonacci sequence up to ~p: ~p~n", [N, Seq]);
        false ->
            handle_error(invalid_input)
    end.

% Function to calculate the sum of the Fibonacci sequence up to the nth number
sum_fib_sequence(N) ->
    case validate_input(N) of
        true ->
            Sum = lists:sum(lists:map(fun fib/1, lists:seq(0, N))),
            Sum;
        false ->
            handle_error(invalid_input)
    end.

% Function to print the sum of the Fibonacci sequence up to the nth number
print_sum_fib_sequence(N) ->
    case validate_input(N) of
        true ->
            Sum = sum_fib_sequence(N),
            io:format("Sum of Fibonacci sequence up to ~p: ~p~n", [N, Sum]);
        false ->
            handle_error(invalid_input)
    end.

% Function to reverse the Fibonacci sequence up to the nth number
reverse_fib_sequence(N) ->
    case validate_input(N) of
        true ->
            Seq = lists:reverse(lists:map(fun fib/1, lists:seq(0, N))),
            Seq;
        false ->
            handle_error(invalid_input)
    end.

% Function to print the reversed Fibonacci sequence up to the nth number
print_reverse_fib_sequence(N) ->
    case validate_input(N) of
        true ->
            Seq = reverse_fib_sequence(N),
            io:format("Reversed Fibonacci sequence up to ~p: ~p~n", [N, Seq]);
        false ->
            handle_error(invalid_input)
    end.

% Start function to create a process and register it if not already registered
% Checks if the process is already registered and reuses it if available
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
% Ensures the process is registered before attempting to stop it
stop() ->
    Pid = whereis(fib_proc),
    if
        Pid =/= undefined -> exit(Pid, normal);
        true -> ok
    end.

% Loop function to handle messages
% Waits for messages to calculate Fibonacci numbers or stop the process
loop() ->
    receive
        {calculate, N} ->
            case validate_input(N) of
                true ->
                    Result = fib(N),
                    io:format("Fibonacci number ~p: ~p~n", [N, Result]),
                    loop();
                false ->
                    handle_error(invalid_input),
                    loop()
            end;
        {calculate_sum, N} ->
            case validate_input(N) of
                true ->
                    Sum = sum_fib_sequence(N),
                    io:format("Sum of Fibonacci sequence up to ~p: ~p~n", [N, Sum]),
                    loop();
                false ->
                    handle_error(invalid_input),
                    loop()
            end;
        {calculate_reverse, N} ->
            case validate_input(N) of
                true ->
                    Seq = reverse_fib_sequence(N),
                    io:format("Reversed Fibonacci sequence up to ~p: ~p~n", [N, Seq]),
                    loop();
                false ->
                    handle_error(invalid_input),
                    loop()
            end;
        stop ->
            io:format("Stopping process~n"),
            ok
    end.