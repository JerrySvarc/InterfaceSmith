# Core Logic

The core logic module implements data-driven UI creation through a hole-based approach. This document describes the key concepts and implementation.

## Hole-Based UI Creation

The system enables incremental UI creation based on concrete data using placeholder "holes":

1. System analyzes JSON input data
2. Creates UI skeleton with hole placeholders
3. Users gradually fill holes with concrete UI elements
4. Structure matches original data hierarchy

### Hole Type Definition

```fsharp
type FieldHole =
    | Named of string  // Holes for specific data fields
    | UnNamed         // Holes for null values
```

## Domain model

### RenderingCode AST
The main type for representing UI elements is **RenderingCode**:
```fsharp
type RenderingCode =
    | HtmlElement of tag: Tag * attrs: Attributes * innerValue: InnerValue * eventHandlers: (string * EventHandler) list
    | HtmlList of listType: ListType * attrs: Attributes * itemCodes: RenderingCode list * eventHandlers: (string * EventHandler) list
    | HtmlObject of objectType: ObjType * attrs: Attributes * keyOrdering: string list * codes: Map<string, RenderingCode> * eventHandlers: (string * EventHandler) list
    | Hole of FieldHole
```

### JSON mapping

The system maps JSON types to RenderingCode:

- JObject → HtmlObject (Unordered collections)
- JArray → HtmlList (Ordered, same-type collections)
- Primitives → HtmlElement
- JNull → Hole UnNamed


## Core operations

### Path-Based Traversal

Operations use integer paths to locate elements in the ASTs:

```
// Example path: [1,0] points to first item in second list
HtmlObject(Div)                     // Path: []
├── "key1" -> HtmlElement(div)     // Path: [0]
└── "key2" -> HtmlList             // Path: [1]
     ├── HtmlElement(Li, "Item 1") // Path: [1,0]
     └── HtmlElement(Li, "Item 2") // Path: [1,1]
```

## Key Operations
1. Replace: Replace element at path with new element
2. Delete: Replace element with Hole
3. Recognize: Map JSON to initial RenderingCode
4. Traverse: Walk both ASTs simultaneously

## Creation Process
1. System visits JSON node
2. Creates Hole placeholder
3. User triggers replacement
4. System creates new element
5. Element integrated into AST
6. Preview updates automatically
