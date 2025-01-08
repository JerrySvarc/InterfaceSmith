# Input Data Guide

## Overview
InterfaceSmith generates UI elements based on JSON data structures. We will now describe how the data provided to the application can be used to create the new UI elements.

As you model/acquire the data you wish to use, make sure the data has a valid structure. Pay attention to Array types, as the system assumes that Arrays contain data of the same type and structure. This is a prerequisite requirement for the system to function correctly.

## JSON to UI Mapping

### Primitive Types

The table below shows the different JSON primitives and their corresponding example UI elements you can create based on this data. However, the examples are really just examples, and you can modify them. The system automatically creates the elements based on the data, and you can specify the tag, attributes, and event handlers using the provided context menus to create the particular elements you want.

| JSON Type | Example UI Element | Example
|-----------|------------|---------|
| String    | Text Input | `"name": "John"`
| Number    | Label | `"age": 25`
| Boolean   | Checkbox | `"active": true`
| Null      | Empty element | `"optional": null`

### Collections
| JSON Type | Example UI Element | Example |
|-----------|------------|---------|
| Object    | Article | `{"user": {...}}` |
| Array     | Ordered List | `"items": [...]` |

The collections are types that contain other data as children. The Object is an *unordered* collection containing different types of data; we assume each type can have a different structure.
However, we assume that the Array is *ordered* and consists of elements of the **same type and structure**.

## Example Input Data

Below we can see an example of data to create a simple TO-DO list application's UI elements.

```js
{
    "InputField": "",
    "AddTodo": "Add todo",
    "Todos": [
        {
            "text": "Complete project proposal",
            "completed": false
        }
    ],
    "Others": {
        "CompletedCount": 0,
        "AllDoneButton": "All done"
    }
}
```
*Fig: Example JSON data*