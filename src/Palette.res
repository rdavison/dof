// A command palette, opened with Escape: a filterable list of commands, where
// a command either runs or opens a further list to choose from. Up and down
// move, Enter chooses, Escape goes back a list and then closes.

@val @scope("document") external body: Dom.element = "body"

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
    switch ReactEvent.Keyboard.key(e) {
    | "ArrowDown" =>
      ReactEvent.Keyboard.preventDefault(e)
      setSelected(i => Js.Math.min_int(i + 1, len - 1))
    | "ArrowUp" =>
      ReactEvent.Keyboard.preventDefault(e)
      setSelected(i => Js.Math.max_int(i - 1, 0))
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
        <ul className="max-h-80 overflow-y-auto py-2">
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
                  onMouseEnter={_ => setSelected(_ => i)}
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
