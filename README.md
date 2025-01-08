# InterfaceSmith                                     
**InterfaceSmith** is a programming system combining a **data-driven** approach to creating web application UIs with a **low-code** programming interface. 

The git [repository](https://github.com/JerrySvarc/InterfaceSmith) is hosted on Github.

The *documentation* is available at: https://jerrysvarc.github.io/InterfaceSmith/
## Installation

To use the InterfaceSmith programming system, we provide two main ways of installing and running it:

1. Using the provided pre-build docker image
2. Building the docker image using the provided Dockerfile

The first option is much faster, as building the image from the Dockerfile involves downloading various technologies and then compiling the entire application and the Docker image, which can take a large amount of time to complete, depending on your internet connection speed and your system's capabilities.

### Prerequisites
- Docker installed and running


### Running the pre-built image
Running the pre-build docker image is the preferred way to run the InterfaceSmith programming system.
To start the container, do the following sequence of steps:

1. Load the provided image:
```bash
docker load < interfacesmith.tar
```

2. Run the loaded container:
```bash
docker run -p 8080:8080 -p 8082:8082 interfacesmith
```

### Building the Dockerfile and running the app

1. Clone the repository:
```bash
git clone https://github.com/JerrySvarc/InterfaceSmith.git
cd InterfaceSmith
```
2. Build the Docker image:
```bash
docker build -t interfacesmith .
```

3. Run the newly built container:
```bash
docker run -p 8080:8080 -p 8082:8082 interfacesmith
```


## **Endpoints**
When the container is **running** the following applications are available:

- Main editor is available on: ```http://localhost:8080```
- This documentation is available on: ```http://localhost:8082```
