# tic-tac-tony
A simple tic tac toe program written in F# inspired by Tony Morris

## The Problem

Here's the [original problem](https://github.com/data61/fp-course/blob/master/projects/TicTacToe/TicTacToe.markdown)
and the associated [blog post](https://blog.tmorris.net/posts/understanding-practical-api-design-static-typing-and-functional-programming/)

## Variations

* [Using discriminated unions and records of functions](https://github.com/artfuldev/tic-tac-tony)
* [Using classes and interfaces](https://github.com/artfuldev/tic-tac-tony/tree/0b756f580fd36faf972ce6538d806fcbfb67921f)

## Running the application

### With dotnet-core

To run tests, run `dotnet test` in `TicTacTony.Tests` directory.

To run the program run `dotnet run --project TicTacTony.Console\TicTacTony.Console.fsproj` in project root.

### With docker

If you do not wish to install dotnet on your platform, you can use docker to run the project.

To build the docker image, run `docker build . -t tic-tac-tony` in project root.

To run the console application, run `docker run -it tic-tac-tony` in project root.
