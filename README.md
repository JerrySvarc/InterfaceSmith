```
██╗███╗   ██╗████████╗███████╗██████╗ ███████╗ █████╗  ██████╗███████╗███████╗███╗   ███╗██╗████████╗██╗  ██╗
██║████╗  ██║╚══██╔══╝██╔════╝██╔══██╗██╔════╝██╔══██╗██╔════╝██╔════╝██╔════╝████╗ ████║██║╚══██╔══╝██║  ██║
██║██╔██╗ ██║   ██║   █████╗  ██████╔╝█████╗  ███████║██║     █████╗  ███████╗██╔████╔██║██║   ██║   ███████║
██║██║╚██╗██║   ██║   ██╔══╝  ██╔══██╗██╔══╝  ██╔══██║██║     ██╔══╝  ╚════██║██║╚██╔╝██║██║   ██║   ██╔══██║
██║██║ ╚████║   ██║   ███████╗██║  ██║██║     ██║  ██║╚██████╗███████╗███████║██║ ╚═╝ ██║██║   ██║   ██║  ██║
╚═╝╚═╝  ╚═══╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝ ╚═════╝╚══════╝╚══════╝╚═╝     ╚═╝╚═╝   ╚═╝   ╚═╝  ╚═╝
```                                                  
**InterfaceSmith** is a programming system combining a **data-driven** approach to creating web application UIs with a **low-code** programming interface. 

## Motivation

**InterfaceSmith** explores a method of creating web applications with a system which guides the programmer during the creation process and provides capabilities that make this process faster and without the need to write large amounts of boilerplate code.
We will explain the differences between the standard way of creating web applications today compared to our prototype system. 

### Standard workflow today
Imagine you aquire some JSON data from a public API and you wish to create an application which displays some of this data, and you also want to add custom functionality for the elements.
The standard method would be to create an empty project in your favourite programming language, analyze the data and then create the UI elements by hand, making sure that each element properly displays the corresponding data.
Then you would also change the styling of the elements using either pure CSS or a library such as Tailwind.
Finally, you would add custom behaviour to react to user events, such as clicking a button or clicking on a dropdown menu.  


### Workflow using InterfaceSmith
Imagine you have the same data as in the previous section. You upload this data to the Data DrUId system, which presents you with a view of the uploaded data.
It also presents you the option to create UI elements based on the data you uploaded. 
You pick and choose which data to use by incrementally creating the UI elements
and modify these elements using the context menus which allow you to change the tag, attributes, and handlers of the element.
You can also style the application using Tailwind and immedeately see the preview of the element after each change. 
Then you can define events and define how the data should change when the event occurs, which in turn changes the content of the UI elements.


## Installation
The preffered method of running the application is by using the provided Dockerfile. 

## Development

### Install pre-requisites
You'll need to install the following pre-requisites in order to build the applications

* [.NET Core SDK](https://www.microsoft.com/net/download) 8.0 or higher
* [Node 20](https://nodejs.org/en/download/)

### Build commands
### 

