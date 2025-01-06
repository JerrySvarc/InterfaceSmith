# Developer Getting Started Guide

## Development Environment Setup

### Prerequisites

1. [Nix package manager](https://nixos.org/download.html) with flakes enabled
2. Docker (optional - for container builds)

### Setting Up Dev Environment

1.  Clone the project repository:
    ```
    git clone https://github.com/JerrySvarc/InterfaceSmith.git
    && cd InterfaceSmith
    ```

2. Enter development shell using flake:
    ```
    nix develop
    ```

    It starts the nix development shell and provides:

    - .NET SDK 8
    - Node.js 20
    - Python with mkdocs
    - Other dev dependencies


## Building the Project
The build system is implemented in Build.fs using FAKE.
Key targets include:

- *Bundle* - Creates production build of the editor
- *Run* - Starts a development server and the documentation server.
- *RunTests* - Runs test suite.
- *BuildDocs* - Builds the documentation
- *Format* - Formats code using *Fantomas*

### Development Workflow

After cloning the project's repository and entering the nix shell, ensure to do the following before the first compilation attempt:

1. Restore .NET tools:

    ```
    dotnet tool restore
    ```

2. Restore Paket dependencies:

    ```
    dotnet paket restore
    ```

Now to start the development server, enter the following command:

```bash
dotnet run
```

It does the following:

- Starts Fable dev server on ```http//localhost:8080```
- Starts documentation server on ```http//localhost:8082```
- Watches for changes and hot reload

### Running Tests

Tests are implemented in the tests project using Fable.Mocha.
To run the tests, input the following command in the nix shell at the root of the project:
```bash
dotnet run RunTests
```
### Building Documentation
Documentation is built using MkDocs with Material theme. Build with:
```bash
dotnet run BuildDocs
```
The documentation source is in Documentation/docs/ and configuration in Documentation/mkdocs.yml.


## Project Structure
The project structure consists of different files and folders.
The following project elements are the most important:

- src/ - Main application code
    - CoreLogic/ - Core domain logic
    - Editor/ - UI components and editor functionality
- tests - Test projects
- Documentation - Documentation source
- Build.fs - Build script
- flake.nix - Development environment definition
