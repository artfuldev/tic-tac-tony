ARG APP_HOME=/usr/app/tic-tac-tony

FROM mcr.microsoft.com/dotnet/core/sdk:3.1 AS build
WORKDIR $APP_HOME/TicTacTony.Core
COPY ./TicTacTony.Core .
RUN dotnet build

FROM mcr.microsoft.com/dotnet/core/sdk:3.1 AS test
COPY --from=build $APP_HOME/TicTacTony.Core $APP_HOME/TicTacTony.Core
COPY ./TicTacTony.Tests $APP_HOME/TicTacTony.Tests
WORKDIR $APP_HOME/TicTacTony.Tests
RUN dotnet test

FROM mcr.microsoft.com/dotnet/core/sdk:3.1 AS publish
COPY --from=build $APP_HOME/TicTacTony.Core $APP_HOME/TicTacTony.Core
COPY ./TicTacTony.Console $APP_HOME/TicTacTony.Console
WORKDIR $APP_HOME/TicTacTony.Console
RUN dotnet publish -c Release -o $APP_HOME/out

FROM mcr.microsoft.com/dotnet/core/runtime:3.1
COPY --from=publish $APP_HOME/out $APP_HOME/out
WORKDIR $APP_HOME/out
ENTRYPOINT dotnet TicTacTony.Console.dll
