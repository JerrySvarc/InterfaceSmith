import { Union, Record } from "./fable_modules/fable-library.3.7.5/Types.js";
import { Reference, RenderingCode, Value, RenderingCode$reflection, Value$reflection } from "./Types.js";
import { union_type, record_type } from "./fable_modules/fable-library.3.7.5/Reflection.js";
import { singleton, ofArray, empty } from "./fable_modules/fable-library.3.7.5/List.js";
import { Cmd_none } from "./fable_modules/Fable.Elmish.3.1.0/cmd.fs.js";
import { createElement } from "react";
import { Interop_reactApi } from "./fable_modules/Feliz.1.62.0/Interop.fs.js";

export class Model extends Record {
    constructor(Data, RenderingCode) {
        super();
        this.Data = Data;
        this.RenderingCode = RenderingCode;
    }
}

export function Model$reflection() {
    return record_type("Index.Model", [], Model, () => [["Data", Value$reflection()], ["RenderingCode", RenderingCode$reflection()]]);
}

export class Msg extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["SetData", "SetRenderingCode"];
    }
}

export function Msg$reflection() {
    return union_type("Index.Msg", [], Msg, () => [[["Item", Value$reflection()]], [["Item", RenderingCode$reflection()]]]);
}

export const example = new RenderingCode(2, ofArray([new RenderingCode(0, "h1", empty(), new Value(2, "TODO list")), new RenderingCode(1, false, new Reference(0, "tasks"), new RenderingCode(3))]));

export const ex2 = new RenderingCode(2, ofArray([new RenderingCode(0, "h1", empty(), new Value(2, "TODO list")), new RenderingCode(1, false, new Reference(0, "tasks"), new RenderingCode(2, ofArray([new RenderingCode(0, "input", singleton(["checked", new Value(0, new Reference(0, "completed"))]), new Value(1)), new RenderingCode(0, "label", empty(), new Value(0, new Reference(0, "task")))])))]));

export function init() {
    return [new Model(new Value(1), example), Cmd_none()];
}

export function update(msg, model) {
    return [new Model(new Value(1), example), Cmd_none()];
}

export function view(model, dispatch) {
    let children_8;
    const children_10 = ofArray([createElement("h1", {
        children: Interop_reactApi.Children.toArray(["TODO list"]),
    }), (children_8 = ofArray([createElement("li", {
        children: Interop_reactApi.Children.toArray(["Buy milk"]),
    }), createElement("li", {
        children: Interop_reactApi.Children.toArray(["Buy eggs"]),
    }), createElement("li", {
        children: Interop_reactApi.Children.toArray(["Buy bread"]),
    })]), createElement("ul", {
        children: Interop_reactApi.Children.toArray(Array.from(children_8)),
    }))]);
    return createElement("div", {
        children: Interop_reactApi.Children.toArray(Array.from(children_10)),
    });
}

//# sourceMappingURL=Index.js.map
