# tic-tac-tony
A simple tic tac toe program written in F# inspired by Tony Morris

## The Problem

Here's the [original problem](https://github.com/data61/fp-course/blob/master/projects/TicTacToe/TicTacToe.markdown)
and the associated [blog post](https://blog.tmorris.net/posts/understanding-practical-api-design-static-typing-and-functional-programming/)

## Approaches

### 1. [Using interfaces and methods](https://github.com/artfuldev/tic-tac-tony/tree/0b756f580fd36faf972ce6538d806fcbfb67921f)

This is the OO approach. The approach is leaky due to interfaces being public,
and now implementations of those interfaces can be external and thereby not
bound to the expected behaviour. Methods are bound to an object and this is very
object oriented because the behavior is actually encapsulated inside the class.
As an example, how to take back a move is actually encoded in the class.

### 2. [Using discriminated unions (which cannot be constructed), and functions](https://github.com/artfuldev/tic-tac-tony/tree/be51b6e4ba303eeac417e783e19dc83720d1c7d9)

This is a better approach. It is very clean, and demonstrates a functional
approach to the problem while showcasing composition-over-inheritance
philosophies and why it is important. It represents the domain closely without
relying on dependent typing. In addition, only valid moves are available to be
played - as moves cannot be constructed externally. Also, it is impossible to
make an invalid move because moves are attached to a particular game and result
on creation and is immutable. Making a move always results in the same new state.

### 3. [Using interfaces (with members of types which cannot be constructed), and functions](https://github.com/artfuldev/tic-tac-tony)

This is an even better approach - here we provide even more information via the
API: for example, `takeBack` takes an `IUndoable` and returns an `IPlayable`,
and both of these are `IGame`s. None of the interfaces can be publicly
implemented by a consumer, because the interfaces have members which cannot be
publicly constructed. We get all the benefits of the previous approach. Even
though it _looks like_ this is object oriented, the API is fully functional
whereby it just deals with objects as data, and all functions operate on data.
This can be easily understood if the internal type constructors are thought of
as functions that create data of a specific form.

## Tests

There are both property-based tests and theories, for all the above approaches.
Kindly note that since the API changes across the approaches, the tests may not
be exactly the same, but similar cases should nonetheless be covered.

Also, property based tests were added a bit later, so some property based tests
may be missing in earlier versions of the source.

## Running the application

### With dotnet-core

To run tests, run `dotnet test` in `TicTacTony.Tests` directory.

To run the program run `dotnet run --project TicTacTony.Console\TicTacTony.Console.fsproj` in project root.

### With docker

If you do not wish to install dotnet on your platform, you can use docker to run the project.

To build the docker image, run `docker build . -t tic-tac-tony` in project root.

To run the console application, run `docker run -it tic-tac-tony` in project root.
