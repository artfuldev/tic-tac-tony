# tic-tac-tony
A simple tic tac toe program written in F# inspired by Tony Morris

## The Problem

Here's the [original problem](https://github.com/data61/fp-course/blob/master/projects/TicTacToe/TicTacToe.markdown)
and the associated [blog post](https://blog.tmorris.net/posts/understanding-practical-api-design-static-typing-and-functional-programming/)

## Variations

### [Using discriminated unions and records of functions](https://github.com/artfuldev/tic-tac-tony)

This is the preferred approach. It is very clean, and demonstrates a functional
approach to the problem while showcasing composition-over-inheritance
philosophies and why it is important. It represents the domain closely without
relying on dependent typing. In addition, only valid moves are available to be
played - as moves cannot be constructed externally. Also, it is impossible to
make an invalid move because moves are attached to a particular game and result
on creation and is immutable. Making a move always results in the same new state.

### [Using classes and interfaces](https://github.com/artfuldev/tic-tac-tony/tree/0b756f580fd36faf972ce6538d806fcbfb67921f)

This is the OO approach. The approach is leaky due to interfaces being public,
and now implementations of those interfaces can be external and thereby not
bound to the expected behaviour.

## Tests

There are both property-based tests and theories.

## Running the application

### With dotnet-core

To run tests, run `dotnet test` in `TicTacTony.Tests` directory.

To run the program run `dotnet run --project TicTacTony.Console\TicTacTony.Console.fsproj` in project root.

### With docker

If you do not wish to install dotnet on your platform, you can use docker to run the project.

To build the docker image, run `docker build . -t tic-tac-tony` in project root.

To run the console application, run `docker run -it tic-tac-tony` in project root.
