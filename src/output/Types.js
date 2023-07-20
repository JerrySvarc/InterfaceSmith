import { Union } from "./fable_modules/fable-library.3.7.5/Types.js";
import { bool_type, list_type, tuple_type, union_type, string_type } from "./fable_modules/fable-library.3.7.5/Reflection.js";

export class Reference extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["Field", "Data"];
    }
}

export function Reference$reflection() {
    return union_type("Types.Reference", [], Reference, () => [[["Item", string_type]], []]);
}

export class Value extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["Reference", "Empty", "Constant"];
    }
}

export function Value$reflection() {
    return union_type("Types.Value", [], Value, () => [[["Item", Reference$reflection()]], [], [["Item", string_type]]]);
}

export class RenderingCode extends Union {
    constructor(tag, ...fields) {
        super();
        this.tag = (tag | 0);
        this.fields = fields;
    }
    cases() {
        return ["HtmlElement", "HtmlList", "Sequence", "Hole"];
    }
}

export function RenderingCode$reflection() {
    return union_type("Types.RenderingCode", [], RenderingCode, () => [[["tag", string_type], ["attrs", list_type(tuple_type(string_type, Value$reflection()))], ["innerText", Value$reflection()]], [["numbered", bool_type], ["innerData", Reference$reflection()], ["itemCode", RenderingCode$reflection()]], [["Item", list_type(RenderingCode$reflection())]], []]);
}

//# sourceMappingURL=Types.js.map
