# InterfaceSmith overview

[InterfaceSmith](https://github.com/JerrySvarc/InterfaceSmith) is a __prototype__ programming system providing a data-driven approach as a main technique to creating web application's UI elements using a **low-code** programming interface.

## Motivation

**InterfaceSmith** explores a method of creating web applications with a system which aims to guide the programmer during the creation process and aims to provide capabilities that make this process faster and without the need to write large amounts of boilerplate code.
We will explain the differences between the standard way of creating web applications using text based code tools compared to our prototype system.

### Standard workflow today
Imagine you aquire some JSON data from a public API and you wish to create an application which displays some of this data, and you also want to add custom functionality for the elements.
The standard method would be to create an empty project in your favourite programming language, analyze the data and then create the UI elements by hand, making sure that each element properly displays the corresponding data.
Then you would also change the styling of the elements using either pure CSS or a library such as Tailwind.
Finally, you would add custom behaviour to react to user events, such as clicking a button or clicking on a dropdown menu.


### Workflow using InterfaceSmith
Imagine you have the same data as in the previous section. You upload this data to the InterfaceSmith system, which presents you with a view of the uploaded data.
It also presents you the option to create UI elements based on the data you uploaded.
You pick and choose which data to use by incrementally creating the UI elements
and modify these elements using the context menus which allow you to change the tag, attributes, and handlers of the element.
You can also style the application using Tailwind and immedeately see the preview of the element after each change.
Then you can define new Elm-style messages and define how the data should change when the message occurs, which in turn changes the application's state and the state of the UI elements.


## Installation

To install the production version, use the provided pre-built docker image and follow the instructions in the [getting-started](getting-started.md) tutorial.


