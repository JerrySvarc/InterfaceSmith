const Msg = {
  Increment: "Increment",
};

const model = {
  Count: 0,
  incrementButton: "Increment counter",
};

const update = (msg, event, model) => {
  switch (msg) {
    case Msg.Increment:
      return {
        ...model,
        Count: model.Count + 1,
      };

    default:
      return model;
  }
};

const view = (model, dispatch) => `
<div  >
<label  >${model.Count}</label>
<button  onClick="window.dispatch(Msg.NewMessage1, event)">${model.incrementButton}</button>
</div>`;

function init(initialModel, updateFunction, viewFunction) {
  let currentModel = initialModel;
  const render = () => {
    const root = document.getElementById("app");
    root.innerHTML = viewFunction(currentModel, dispatch);
  };
  window.dispatch = (msg, event) => {
    currentModel = updateFunction(msg, event, currentModel);
    render();
  };
  render();
}

init(model, update, view);
