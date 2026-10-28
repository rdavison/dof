// The layout editor's pieces that know nothing of layouts: dragging one key
// onto another, the key properties panel, and the dialogs and toast around
// export and import. Keys are named by KeyboardEvent code throughout.

type point = {x: float, y: float}

// What a key on the board needs while the editor is open. Outside the editor
// the context is None and keys behave as they always have.
type handlers = {
  // whether the key can be dragged and edited
  editable: string => bool,
  selected: option<string>,
  // the key being dragged once it has moved, and the key under the pointer
  source: option<string>,
  target: option<string>,
  onPointerDown: (string, ReactEvent.Pointer.t) => unit,
  // a colour to fill the key with, such as its finger's
  fill: string => option<string>,
}

let context: React.Context.t<option<handlers>> = React.createContext(None)

module Provider = {
  let provider = React.Context.provider(context)

  @react.component
  let make = (~value, ~children) =>
    React.createElement(provider, {"value": value, "children": children})
}

@val @scope("document")
external elementFromPoint: (float, float) => Js.Nullable.t<Dom.element> = "elementFromPoint"
@send external closest: (Dom.element, string) => Js.Nullable.t<Dom.element> = "closest"
@send external getAttribute: (Dom.element, string) => Js.Nullable.t<string> = "getAttribute"
@get external clientX: Dom.event => float = "clientX"
@get external clientY: Dom.event => float = "clientY"

// The editable key under a point, read off its data-key attribute.
let keyAt = ({x, y}) =>
  elementFromPoint(x, y)
  ->Js.Nullable.toOption
  ->Option.flatMap(el => el->closest("[data-key]")->Js.Nullable.toOption)
  ->Option.flatMap(el => el->getAttribute("data-key")->Js.Nullable.toOption)

type drag = {
  code: string,
  start: point,
  at: point,
  // past the few pixels that still count as a click
  moved: bool,
  target: option<string>,
}

// Press on a key and release: a click, which selects it. Press, move and
// release over another key: a swap. Release anywhere else: nothing.
let useDrag = (~onSwap, ~onSelect) => {
  let (drag, setDrag) = React.useState(() => None)
  // The window listeners outlive renders, so they read the latest state and
  // callbacks through refs.
  let current = React.useRef(None)
  let callbacks = React.useRef((onSwap, onSelect))
  callbacks.current = (onSwap, onSelect)
  let set = d => {
    current.current = d
    setDrag(_ => d)
  }

  React.useEffect0(() => {
    let onMove = ev =>
      switch current.current {
      | Some(d) =>
        let at = {x: clientX(ev), y: clientY(ev)}
        let far = Js.Math.abs_float(at.x -. d.start.x) +. Js.Math.abs_float(at.y -. d.start.y) > 4.
        let moved = d.moved || far
        set(Some({...d, at, moved, target: moved ? keyAt(at) : None}))
      | None => ()
      }
    let onUp = _ =>
      switch current.current {
      | Some(d) =>
        set(None)
        let (onSwap, onSelect) = callbacks.current
        switch (d.moved, d.target) {
        | (false, _) => onSelect(d.code)
        | (true, Some(target)) if target != d.code => onSwap(d.code, target)
        | _ => ()
        }
      | None => ()
      }
    let w = Webapi.Dom.window
    w->Webapi.Dom.Window.addEventListener("pointermove", onMove)
    w->Webapi.Dom.Window.addEventListener("pointerup", onUp)
    w->Webapi.Dom.Window.addEventListener("pointercancel", onUp)
    Some(
      () => {
        w->Webapi.Dom.Window.removeEventListener("pointermove", onMove)
        w->Webapi.Dom.Window.removeEventListener("pointerup", onUp)
        w->Webapi.Dom.Window.removeEventListener("pointercancel", onUp)
      },
    )
  })

  let onPointerDown = (code, e) =>
    if ReactEvent.Pointer.button(e) == 0 {
      // no text selection while dragging
      ReactEvent.Pointer.preventDefault(e)
      let at = {x: ReactEvent.Pointer.clientX(e)->Int.toFloat, y: ReactEvent.Pointer.clientY(e)->Int.toFloat}
      set(Some({code, start: at, at, moved: false, target: None}))
    }

  (drag, onPointerDown)
}

// Press on a key and drag across others: each key the pointer crosses is
// painted once, as with a brush. Returns the handler for a key's pointer-down.
let usePaint = (~onPaint) => {
  // the last key painted while the button is down
  let last = React.useRef(None)
  let callback = React.useRef(onPaint)
  callback.current = onPaint

  React.useEffect0(() => {
    let onMove = ev =>
      switch (last.current, keyAt({x: clientX(ev), y: clientY(ev)})) {
      | (Some(previous), Some(code)) if code != previous =>
        last.current = Some(code)
        callback.current(code)
      | _ => ()
      }
    let onUp = _ => last.current = None
    let w = Webapi.Dom.window
    w->Webapi.Dom.Window.addEventListener("pointermove", onMove)
    w->Webapi.Dom.Window.addEventListener("pointerup", onUp)
    w->Webapi.Dom.Window.addEventListener("pointercancel", onUp)
    Some(
      () => {
        w->Webapi.Dom.Window.removeEventListener("pointermove", onMove)
        w->Webapi.Dom.Window.removeEventListener("pointerup", onUp)
        w->Webapi.Dom.Window.removeEventListener("pointercancel", onUp)
      },
    )
  })

  (code, e) =>
    if ReactEvent.Pointer.button(e) == 0 {
      ReactEvent.Pointer.preventDefault(e)
      last.current = Some(code)
      onPaint(code)
    }
}

// The dragged key, following the pointer. It ignores the pointer itself, so
// the key underneath can still be found.
module Ghost = {
  @react.component
  let make = (~at: point, ~label: array<string>) =>
    <div
      className="pointer-events-none fixed z-40 flex h-[60px] w-[60px] -translate-x-1/2 -translate-y-1/2 items-center rounded-lg bg-slate-100 text-slate-900 shadow-2xl shadow-black ring-2 ring-sky-400"
      style={ReactDOM.Style.make(
        ~left=`${Float.toString(at.x)}px`,
        ~top=`${Float.toString(at.y)}px`,
        (),
      )}>
      <div className="m-auto flex flex-col items-center">
        {label->Array.length == 2
          ? <>
              <div> {React.string(label->Array.getUnsafe(1))} </div>
              <div> {React.string(label->Array.getUnsafe(0))} </div>
            </>
          : React.string(label->Array.get(0)->Option.getWithDefault(""))}
      </div>
    </div>
}

// The last character in a field, so typing into a filled one-character field
// replaces what was there.
let lastChar: string => string = %raw(`s => { const cs = [...s]; return cs.length ? cs[cs.length - 1] : "" }`)

module Panel = {
  module Field = {
    @react.component
    let make = (~label, ~value, ~also: option<string>, ~onChange, ~onClose) =>
      <label className="flex flex-col gap-1">
        <span className="text-xs text-slate-400"> {React.string(label)} </span>
        <input
          className="w-16 rounded bg-slate-900 px-3 py-2 text-center text-2xl text-white outline-none ring-1 ring-white/10 focus:ring-sky-400"
          value
          onFocus={e => ReactEvent.Focus.target(e)["select"](.)}
          onChange={e => {
            let c = lastChar(ReactEvent.Form.target(e)["value"])
            // a key always types something, so clearing the field keeps it
            if c != "" {
              onChange(c)
            }
          }}
          onKeyDown={e =>
            if ReactEvent.Keyboard.key(e) == "Escape" {
              ReactEvent.Keyboard.preventDefault(e)
              onClose()
            }}
        />
        <span className="h-4 text-xs text-amber-300">
          {React.string(
            switch also {
            | Some(code) => `also on ${code}`
            | None => ""
            },
          )}
        </span>
      </label>
  }

  // `also` names another key that carries the same character, if any.
  @react.component
  let make = (~code, ~base, ~shift, ~also: string => option<string>, ~onChange, ~onClose) =>
    <div
      className="flex items-start gap-6 rounded-xl bg-slate-800 px-6 py-4 font-mono text-sm text-slate-200 shadow-2xl shadow-black ring-1 ring-white/10">
      <div className="flex flex-col gap-1">
        <span className="text-xs text-slate-400"> {React.string("Key")} </span>
        <span className="py-2 text-lg"> {React.string(code)} </span>
      </div>
      <Field
        label="Base" value=base also={also(base)} onChange={c => onChange(c, shift)} onClose
      />
      <Field
        label="Shift" value=shift also={also(shift)} onChange={c => onChange(base, c)} onClose
      />
      <button
        className="ml-auto rounded px-2 py-1 text-slate-400 hover:bg-slate-700 hover:text-white"
        onClick={_ => onClose()}>
        {React.string("Close")}
      </button>
    </div>
}

// A small window over the page, styled like the command palette. Escape or a
// click outside closes it.
module Dialog = {
  @react.component
  let make = (~title, ~onClose, ~children) =>
    <div
      className="fixed inset-0 z-50 flex items-start justify-center bg-black/50 pt-[15vh]"
      onMouseDown={_ => onClose()}>
      <div
        className="flex w-[560px] max-w-[calc(100vw-32px)] flex-col gap-3 rounded-xl bg-slate-800 p-4 font-mono text-sm text-slate-200 shadow-2xl shadow-black ring-1 ring-white/10"
        onMouseDown={e => ReactEvent.Mouse.stopPropagation(e)}
        onKeyDown={e =>
          if ReactEvent.Keyboard.key(e) == "Escape" {
            ReactEvent.Keyboard.preventDefault(e)
            onClose()
          }}>
        <div className="text-slate-400"> {React.string(title)} </div>
        children
      </div>
    </div>
}

module Toast = {
  @react.component
  let make = (~message) =>
    <div
      className="fixed bottom-8 left-1/2 z-50 -translate-x-1/2 rounded-lg bg-slate-800 px-4 py-2 font-mono text-sm text-slate-100 shadow-2xl shadow-black ring-1 ring-white/10">
      {React.string(message)}
    </div>
}
