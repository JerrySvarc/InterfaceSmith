# Architecture

InterfaceSmith follows a nested Elmish architecture approach with two main modules:

1. Core Logic Module
2. Editor Module

## Main Application Architecture

The main application is implemented as an Elmish application responsible for general functionality like Page management. Each Page is managed by its own Elmish PageEditor sub-application.

### Main Application State

```fsharp
type Model = {
    Pages: Map<Guid, PageEditorModel>
    PageOrder: Guid list
    ActivePageId: Guid option
    IsSidebarOpen: bool
}
```

### Main Application Messages

```fsharp
type Msg =
    | CreatePage
    | UpdatePage of PageEditorModel
    | DeletePage of Guid
    | ToggleSidebar
    | OpenPage of Guid
    | PageEditorMsg of Guid * PageEditorMsg
```

## PageEditor Sub-Application

The PageEditor provides modification functionality for individual Pages with:

- Local state (PageEditorModel)
- Update logic (pageEditorUpdate)
- View components (PageEditorView)

### PageEditor State

```fsharp
type PageEditorModel = {
    PageData: Page
    FileUploadError: bool
    ViewportPosition: Position
    Scale: float
    Elements: Element list
    DraggingElementId: int option
    IsPanning: bool
    LastMousePosition: Position option
    IsPreviewOpen: bool
    ContextMenuVisible: bool
    ContextMenuPosition: Position option
}
```

## Module Structure

##### Core Logic Module

Contains core domain logic and operations:

- **Types/**

    - `RenderingTypes.fs` - Core domain types

- **Operations/**

    - `RenderingCode.fs` - AST operations
  - `DataRecognition.fs` - JSON to UI mapping
  - `CodeGeneration.fs` - Code generation

### Editor Module

Contains UI implementation:

- **Types/**

    - `EditorDomain.fs` - Main app types
  - `PageEditorDomain.fs` - PageEditor types

- **Utilities/**

     - Icons
  - File upload
  - JSON parsing
  - JavaScript editor

- **Components/**

    - `ElementComponents.fs` - Canvas elements
  - `OptionsComponents.fs` - Context menus
  - `EditorComponents.fs` - Editor UI
  - `PageEditorComponents.fs` - PageEditor implementation

- **CustomRendering/**

    - Preview rendering
  - Modification menus
  - Canvas elements

## Key Design Principles

1. Separation of concerns between main app and PageEditors
2. Domain types separated from operations
3. Core logic independent from UI implementation
4. Modular component-based UI architecture
5. Nested elemish application structure