const Msg = {
    UpdateCelsius: "UpdateCelsius",
    UpdateFahrenheit: "UpdateFahrenheit",
};

const model = {
    Celsius: "",
    CelsiusLabel: "Celsius = ",
    Fahrenheit: "",
    FahrenheitLabel: "Fahrenheit",
};

const update = (msg, event, model) => {
    switch (msg) {
        case Msg.UpdateCelsius:
            if (isNaN(parseFloat(event.target.value))) {
                return {
                    ...model,
                    Celsius: event.target.value,
                };
            }
            const celsius = parseFloat(event.target.value);
            const fahrenheit = (celsius * 9) / 5 + 32;
            return {
                ...model,
                Celsius: celsius,
                Fahrenheit: fahrenheit.toString(),
            };

        case Msg.UpdateFahrenheit:
            if (isNaN(parseFloat(event.target.value))) {
                return {
                    ...model,
                    Fahrenheit: event.target.value,
                };
            }
            const fahrenheit2 = parseFloat(event.target.value);
            const celsius2 = ((fahrenheit2 - 32) * 5) / 9;
            return {
                ...model,
                Fahrenheit: fahrenheit,
                Celsius: celsius2.toString(),
            };

        default:
            return model;
    }
};

const view = (
    model,
    dispatch,
) =>
    `<div  ><input value="${model.Celsius}" onChange="window.dispatch(Msg.UpdateCelsius, event)" />
<label  >${model.CelsiusLabel}</label>
<input value="${model.Fahrenheit}" onChange="window.dispatch(Msg.UpdateFahrenheit, event)" />
<label  >${model.FahrenheitLabel}</label></div>`;

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
