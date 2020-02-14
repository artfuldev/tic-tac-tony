# tic-tac-tony
A simple tic tac toe program written in F# inspired by Tony Morris

## Getting Started

### With dotnet-core

To run tests, run `dotnet test` in `TicTacTony.Tests` directory.

To run the program run `dotnet run --project TicTacTony.Console\TicTacTony.Console.fsproj` in project root.

### With docker

If you do not wish to install dotnet on your platform, you can use docker to run the project.

To build the docker image, run `docker build . -t tic-tac-tony` in project root.

To run the console application, run `docker run -it tic-tac-tony` in project root.
