# Overview

My name is Maxim Person and as a software engineer I aim to deepen my understanding of functional programming by developing a Fibonacci sequence generator using Erlang. This project demonstrates the core concepts of the Erlang language, such as recursion, pattern matching, process creation, and messaging.

The purpose of this software is to calculate and display the Fibonacci sequence up to a specified number, showcasing the functional programming principles in Erlang. This generator can handle sequences up to the 100th Fibonacci number. Additionally, it can calculate the sum of the sequence and generate the sequence in reverse order.

PUT VIDEO HERE

# Development Environment

- **Development Tools**: Visual Studio Code with Erlang extension
- **Programming Language**: Erlang/OTP 27
- **Libraries**: No additional libraries were used

# Useful Websites

* [Erlang Official Website](https://www.erlang.org/)
* [Erlang Standard Library Reference Manual](https://erlang.org/doc/)
* [Erlang by Example](https://erlangbyexample.org/)
* [Programming Erlang (2nd Edition) - O'Reilly](https://www.oreilly.com/library/view/programming-erlang-2nd/9781941222454/)
* [Erlang Standard Library API Reference](https://www.erlang.org/doc/apps/stdlib/api-reference.html)
* [Functional Programming - Wikipedia](https://en.wikipedia.org/wiki/Functional_programming)

# Features

- **Fibonacci Sequence Generation**: Calculates the Fibonacci sequence up to a specified number using tail recursion for efficiency.
- **Sum Calculation**: Calculates the sum of the Fibonacci sequence up to a specified number.
- **Reverse Sequence Generation**: Generates the Fibonacci sequence in reverse order.
- **Process Creation and Messaging**: Demonstrates process creation and message passing in Erlang.
- **Input Validation**: Ensures that the input is a non-negative integer.
- **Error Handling**: Handles invalid input and unknown errors gracefully.

# Usage

1. **Compile the code**: `erlc fibonacci.erl`
2. **Start the Erlang shell**: `erl`
3. **Load the module**: `c(fibonacci).`
4. **Print the Fibonacci sequence up to the 100th number**: `fibonacci:print_fib_sequence(100).`
5. **Print the sum of the Fibonacci sequence up to the 100th number**: `fibonacci:print_sum_fib_sequence(100).`
6. **Print the reversed Fibonacci sequence up to the 100th number**: `fibonacci:print_reverse_fib_sequence(100).`
7. **Start the process**: `Pid = fibonacci:start().`
8. **Calculate a Fibonacci number using the process**: `Pid ! {calculate, 100}.`
9. **Calculate and print the sum of the Fibonacci sequence using the process**: `Pid ! {calculate_sum, 100}.`
10. **Calculate and print the reversed Fibonacci sequence using the process**: `Pid ! {calculate_reverse, 100}.`
11. **Stop the process**: `Pid ! stop.`

# Future Work

* Implement additional error handling and input validation
* Enhance the user interface for better interaction
* Optimize the Fibonacci sequence algorithm for larger numbers
