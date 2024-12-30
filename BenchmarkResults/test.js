const Msg = {
  UpdateInput: "UpdateInput",
  AddTodo: "AddTodo",
  ToggleTodo: "ToggleTodo",
  CompleteAll: "CompleteAll",
};

const model = {
  InputField: "",
  AddTodo: "Add todo",
  Todos: [
    {
      text: "Complete project proposal",
      completed: false,
    },
  ],
  Others: {
    CompletedCount: 0,
    AllDoneButton: "All done",
  },
};

const update = (msg, event, model) => {
  switch (msg) {
    case Msg.UpdateInput:
      return {
        ...model,
        InputField: event.target.value,
      };

    case Msg.AddTodo:
      if (!model.InputField.trim()) return model;
      return {
        ...model,
        InputField: "",
        Todos: [
          ...model.Todos,
          {
            text: model.InputField.trim(),
            completed: false,
          },
        ],
      };

    case Msg.ToggleTodo:
      const todoIndex = parseInt(event.target.closest("li").dataset.index);
      const updatedTodos = model.Todos.map((todo, index) =>
        index === todoIndex
          ? {
              ...todo,
              completed: !todo.completed,
            }
          : todo,
      );
      return {
        ...model,
        Todos: updatedTodos,
        Others: {
          ...model.Others,
          CompletedCount: updatedTodos.filter((todo) => todo.completed).length,
        },
      };

    case Msg.CompleteAll:
      const allCompleted = model.Todos.map((todo) => ({
        ...todo,
        completed: true,
      }));
      return {
        ...model,
        Todos: allCompleted,
        Others: {
          ...model.Others,
          CompletedCount: allCompleted.length,
        },
      };

    default:
      return model;
  }
};

const view = (
  model,
) => `<div  ><input value="${model.InputField}" type="text" onBlur="window.dispatch(Msg.NewMessage1, event)" />
<button  onClick="window.dispatch(Msg.NewMessage2, event)">${model.AddTodo}</button>
<ul  >${model.Todos.map(
  (item, index) => `
                        <li data-index="${index}"><div  ><input type="checkbox" ${item.completed ? "checked" : ""} onChange="window.dispatch(Msg.NewMessage3, event)" />
<span  >${item.text}</span></div></li>`,
).join("")}</ul>
<div  ><button  onClick="window.dispatch(Msg.NewMessage4, event)">${model.Others.AllDoneButton}</button>
<div  >${model.Others.CompletedCount}</div></div></div>`;

function init(initialModel, updateFunction, viewFunction) {
  let currentModel = initialModel;
  const render = () => {
    const root = document.getElementById("app");
    root.innerHTML = view(currentModel, dispatch);
  };
  window.dispatch = (msg, event) => {
    currentModel = update(msg, event, currentModel);
    render();
  };
  render();
}

init(model, update, view);
