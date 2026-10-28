// A command palette, opened with Escape: a filterable list of commands, where
// a command either runs or opens a further list to choose from. Up and down
// move (Page Up and Page Down by a screenful, Home and End to either end),
// Enter chooses, Escape goes back a list and then closes.

@val @scope("document") external body: Dom.element = "body"
@get external children: Dom.element => Js.Array2.array_like<Dom.element> = "children"
@get external offsetTop: Dom.element => int = "offsetTop"
@get external offsetHeight: Dom.element => int = "offsetHeight"
@get external clientHeight: Dom.element => int = "clientHeight"
@get external scrollTop: Dom.element => int = "scrollTop"
@get external scrollHeight: Dom.element => int = "scrollHeight"
@set external setScrollTop: (Dom.element, int) => unit = "scrollTop"

type rec item = {
  label: string,
  // dimmed on the right, such as the current setting
  hint: string,
  // marks the current choice in a list of options
  checked: bool,
  action: action,
}
and action =
  | Run(unit => unit)
  | Open(page)
and page = {title: string, placeholder: string, items: array<item>}

let matches = (item, query) =>
  item.label->String.toLowerCase->String.includes(query->String.trim->String.toLowerCase)

@react.component
let make = (~root: page) => {
  let (isOpen, setOpen) = React.useState(() => false)
  // pages opened from the root, newest first
  let (stack, setStack) = React.useState(() => list{})
  let (query, setQuery) = React.useState(() => "")
  let (selected, setSelected) = React.useState(() => 0)

  let page = switch stack {
  | list{p, ..._} => p
  | list{} => root
  }
  let atRoot = switch stack {
  | list{} => true
  | _ => false
  }
  let items = page.items->Array.filter(i => matches(i, query))

  let close = () => setOpen(_ => false)

  // The list scrolls once it outgrows its height, and has to follow the
  // selection as the keys move it.
  let list = React.useRef(Js.Nullable.null)
  let row = i =>
    list.current
    ->Js.Nullable.toOption
    ->Option.flatMap(ul => Js.Array2.from(children(ul))->Array.get(i)->Option.map(li => (ul, li)))

  React.useEffect3(() => {
    switch row(selected) {
    // the ends go all the way, past the list's padding
    | Some((ul, _)) if selected == 0 => ul->setScrollTop(0)
    | Some((ul, _)) if selected == items->Array.length - 1 => ul->setScrollTop(scrollHeight(ul))
    | Some((ul, li)) =>
      let top = offsetTop(li)
      let bottom = top + offsetHeight(li)
      if top < scrollTop(ul) {
        ul->setScrollTop(top)
      } else if bottom > scrollTop(ul) + clientHeight(ul) {
        ul->setScrollTop(bottom - clientHeight(ul))
      }
    | None => ()
    }
    None
  }, (selected, query, stack))

  // how many rows fit in the list, for Page Up and Page Down
  let pageSize = () =>
    switch row(0) {
    | Some((ul, li)) if offsetHeight(li) > 0 =>
      Js.Math.max_int(1, clientHeight(ul) / offsetHeight(li) - 1)
    | _ => 1
    }

  React.useEffect0(() => {
    module K = Webapi.Dom.KeyboardEvent
    // Only an Escape pressed on the page opens it. One pressed in the palette's
    // own input is handled there, and must not reopen the palette it closed.
    let onKey = ev =>
      if K.code(ev) == "Escape" && Obj.magic(K.target(ev)) === body {
        K.preventDefault(ev)
        setStack(_ => list{})
        setQuery(_ => "")
        setSelected(_ => 0)
        setOpen(_ => true)
      }
    Webapi.Dom.window->Webapi.Dom.Window.addKeyDownEventListener(onKey)
    Some(() => Webapi.Dom.window->Webapi.Dom.Window.removeKeyDownEventListener(onKey))
  })

  let back = () =>
    switch stack {
    | list{_, ...rest} =>
      setStack(_ => rest)
      setQuery(_ => "")
      setSelected(_ => 0)
    | list{} => close()
    }

  let choose = item =>
    switch item.action {
    | Run(f) =>
      close()
      f()
    | Open(p) =>
      setStack(s => list{p, ...s})
      setQuery(_ => "")
      // start on the current choice
      setSelected(_ => Js.Math.max_int(0, p.items->Array.findIndex(i => i.checked)))
    }

  let onKeyDown = e => {
    let len = items->Array.length
    let move = f => {
      ReactEvent.Keyboard.preventDefault(e)
      setSelected(i => Js.Math.max_int(0, Js.Math.min_int(f(i), len - 1)))
    }
    switch ReactEvent.Keyboard.key(e) {
    | "ArrowDown" => move(i => i + 1)
    | "ArrowUp" => move(i => i - 1)
    | "PageDown" =>
      let n = pageSize()
      move(i => i + n)
    | "PageUp" =>
      let n = pageSize()
      move(i => i - n)
    | "Home" => move(_ => 0)
    | "End" => move(_ => len - 1)
    | "Enter" =>
      ReactEvent.Keyboard.preventDefault(e)
      items->Array.get(selected)->Option.forEach(choose)
    | "Escape" =>
      ReactEvent.Keyboard.preventDefault(e)
      back()
    | "Backspace" if query == "" && !atRoot =>
      ReactEvent.Keyboard.preventDefault(e)
      back()
    | _ => ()
    }
  }

  if !isOpen {
    React.null
  } else {
    <div
      className="fixed inset-0 z-50 flex items-start justify-center bg-black/50 pt-[15vh]"
      onMouseDown={_ => close()}>
      <div
        className="w-[560px] max-w-[calc(100vw-32px)] overflow-hidden rounded-xl bg-slate-800 font-mono text-sm text-slate-200 shadow-2xl shadow-black ring-1 ring-white/10"
        onMouseDown={e => ReactEvent.Mouse.stopPropagation(e)}>
        <div className="flex items-center gap-2 border-b border-white/10 px-4">
          {switch stack {
          | list{p, ..._} =>
            <span className="rounded bg-slate-700 px-2 py-0.5 text-xs text-slate-300">
              {React.string(p.title)}
            </span>
          | list{} => React.null
          }}
          <input
            autoFocus=true
            className="flex-1 bg-transparent py-3 text-base text-slate-100 placeholder-slate-500 outline-none"
            placeholder=page.placeholder
            value=query
            onChange={e => {
              let value = ReactEvent.Form.target(e)["value"]
              setQuery(_ => value)
              setSelected(_ => 0)
            }}
            onKeyDown
          />
        </div>
        <ul ref={ReactDOM.Ref.domRef(list)} className="relative max-h-80 overflow-y-auto py-2">
          {if items->Array.length == 0 {
            <li className="px-4 py-2 text-slate-500"> {React.string("No matches")} </li>
          } else {
            React.array(
              items->Array.mapWithIndex((item, i) =>
                <li
                  key=item.label
                  className={"flex cursor-pointer justify-between px-4 py-2 " ++ (
                    i == selected ? "bg-slate-700 text-white" : "text-slate-300"
                  )}
                  // Only a moving pointer selects: the list scrolling under a
                  // resting one mustn't take the selection from the keys.
                  onMouseMove={_ =>
                    if i != selected {
                      setSelected(_ => i)
                    }}
                  onMouseDown={e => {
                    // keep focus in the input
                    ReactEvent.Mouse.preventDefault(e)
                    choose(item)
                  }}>
                  <span> {React.string(item.label)} </span>
                  <span className="flex gap-3 text-slate-500">
                    <span> {React.string(item.hint)} </span>
                    {item.checked ? <span> {React.string("✓")} </span> : React.null}
                  </span>
                </li>
              ),
            )
          }}
        </ul>
        <div className="border-t border-white/10 px-4 py-2 text-xs text-slate-500">
          {React.string(
            "↑↓ move · ↵ choose · esc " ++ (atRoot ? "close" : "back"),
          )}
        </div>
      </div>
    </div>
  }
}
