# Value-driven-UI
A program for creating web applications based on concrete values,
The specification for the project can be found [here](project-specification.pdf).


## Install pre-requisites

You'll need to install the following pre-requisites in order to build the applications

* [.NET Core SDK](https://www.microsoft.com/net/download) 6.0 or higher
* [Node 16](https://nodejs.org/en/download/)

## Starting the application

Before you run the project **for the first time only** you must install dotnet "local tools" with this command:

```bash
dotnet tool restore
```

To concurrently run the server and the client components in watch mode use the following command:

```bash
dotnet run
```

Then open `http://localhost:8080` in your browser.

The build project in root directory contains a couple of different build targets. You can specify them after `--` (target name is case-insensitive).

To run concurrently server and client tests in watch mode (you can run this command in parallel to the previous one in new terminal):

```bash
dotnet run -- Bundle
```

Alternatively, you can use the Nix package manager and enter the development shell using this command:
```nix
nix develop
```
