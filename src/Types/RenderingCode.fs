module RenderingCode

open Types.RenderingTypes
let rec addCode path newCode = 0

let rec replace (path: int list) (replacementElement : RenderingCode) (currentCode: RenderingCode) =
    match path with
    | [] ->
         replacementElement
    | head :: tail ->
        match currentCode with
        | HtmlList(lt, items, handlers) ->

            let newItems =
                match items with
                | [] -> items
                | _ ->
                    if head < items.Length then
                        items |> List.map (replace tail replacementElement)
                    else
                        items
            HtmlList(lt, newItems, handlers)
        | HtmlObject( objType, keys, items, handlers) ->
            match List.tryItem head keys with
            | Some key ->
                let value = items.TryFind key
                match value with
                | Some code ->
                    let newCode =
                        replace tail replacementElement code
                    let newItems = items.Add (key, newCode)
                    HtmlObject(objType, keys, newItems, handlers)
                | None ->
                    HtmlObject(objType, keys, items, handlers)
            | None ->
                HtmlObject(objType, keys, items, handlers)
        | CustomWrapper(customWrapper) ->
            match head with
            | -1 ->
                let newItems =
                    match customWrapper.Children with
                    | [] -> customWrapper.Children
                    | _ -> customWrapper.Children |> List.mapi (fun i item ->
                        if tail.Head = i then
                            replace tail.Tail replacementElement item
                        else
                            item
                        )
                CustomWrapper({customWrapper with Children = newItems })
            | _ ->
                let newCode = replace tail replacementElement customWrapper.WrappedCode
                CustomWrapper({customWrapper with WrappedCode = newCode  })
        | _ -> currentCode
