module Key = {
  type shape = [
    | #_1
    | #_1_25
    | #_1_5
    | #_1_75
    | #_2
    | #_2_25
    | #_5
    | #Nav
  ]

  @react.component
  let make = (~id="", ~label: array<string>, ~shape: shape) => {
    let shape = {
      switch shape {
      | #_1 => "h-[60px] w-[60px]"
      | #_1_25 => "h-[60px] w-[77px]"
      | #_1_5 => "h-[60px] w-[94px]"
      | #_1_75 => "h-[60px] w-[109px]"
      | #_2 => "h-[60px] w-[124px]"
      | #_2_25 => "h-[60px] w-[141px]"
      | #_5 => "h-[60px] w-[316px]"
      | #Nav => "h-[60px] w-[188px]"
      }
    }
    let label = {
      switch label {
      | [x] => React.string(x)
      | [x, y] =>
        <div className="flex flex-col">
          <div> {React.string(y)} </div>
          <div> {React.string(x)} </div>
        </div>
      | _ => failwith("")
      }
    }
    <div
      id
      className={`${shape} m-[2px] bg-slate-900 hover:bg-slate-100 transition-colors rounded-lg shadow-2xl shadow-slate-700 text-white ease-out hover:duration-75 duration-[2000ms] hover:text-slate-900 items-center flex`}>
      <div className="m-auto"> {label} </div>
    </div>
  }
}

module Ansi = {
  type t = {
    tilde: array<string>,
    one: array<string>,
    two: array<string>,
    three: array<string>,
    four: array<string>,
    five: array<string>,
    six: array<string>,
    seven: array<string>,
    eight: array<string>,
    nine: array<string>,
    zero: array<string>,
    hyphen: array<string>,
    equal: array<string>,
    backspace: array<string>,
    tab: array<string>,
    q: array<string>,
    w: array<string>,
    e: array<string>,
    r: array<string>,
    t: array<string>,
    y: array<string>,
    u: array<string>,
    i: array<string>,
    o: array<string>,
    p: array<string>,
    open_bracket: array<string>,
    close_bracket: array<string>,
    backslash: array<string>,
    caps_lock: array<string>,
    a: array<string>,
    s: array<string>,
    d: array<string>,
    f: array<string>,
    g: array<string>,
    h: array<string>,
    j: array<string>,
    k: array<string>,
    l: array<string>,
    semicolon: array<string>,
    quote: array<string>,
    enter: array<string>,
    left_shift: array<string>,
    z: array<string>,
    x: array<string>,
    c: array<string>,
    v: array<string>,
    b: array<string>,
    n: array<string>,
    m: array<string>,
    comma: array<string>,
    period: array<string>,
    slash: array<string>,
    right_shift: array<string>,
  }

  let qwerty = {
    tilde: ["`", "~"],
    one: ["1", "!"],
    two: ["2", "@"],
    three: ["3", "#"],
    four: ["4", "$"],
    five: ["5", "%"],
    six: ["6", "^"],
    seven: ["7", "&"],
    eight: ["8", "*"],
    nine: ["9", "("],
    zero: ["0", ")"],
    hyphen: ["-", "_"],
    equal: ["=", "+"],
    backspace: ["delete"],
    tab: ["tab"],
    q: ["Q"],
    w: ["W"],
    e: ["E"],
    r: ["R"],
    t: ["T"],
    y: ["Y"],
    u: ["U"],
    i: ["I"],
    o: ["O"],
    p: ["P"],
    open_bracket: ["[", "{"],
    close_bracket: ["]", "}"],
    backslash: ["\\", "|"],
    caps_lock: ["caps"],
    a: ["A"],
    s: ["S"],
    d: ["D"],
    f: ["F"],
    g: ["G"],
    h: ["H"],
    j: ["J"],
    k: ["K"],
    l: ["L"],
    semicolon: [";", ":"],
    quote: ["'", "\""],
    enter: ["return"],
    left_shift: ["shift"],
    z: ["Z"],
    x: ["X"],
    c: ["C"],
    v: ["V"],
    b: ["B"],
    n: ["N"],
    m: ["M"],
    comma: [",", "<"],
    period: [".", ">"],
    slash: ["/", "?"],
    right_shift: ["shift"],
  }

  let graphite = {
    tilde: ["`", "~"],
    one: ["1", "!"],
    two: ["2", "@"],
    three: ["3", "#"],
    four: ["4", "$"],
    five: ["5", "%"],
    six: ["6", "^"],
    seven: ["7", "&"],
    eight: ["8", "*"],
    nine: ["9", "("],
    zero: ["0", ")"],
    hyphen: ["[", "{"],
    equal: ["]", "}"],
    backspace: ["delete"],
    tab: ["tab"],
    q: ["B"],
    w: ["L"],
    e: ["D"],
    r: ["W"],
    t: ["Z"],
    y: ["'", "_"],
    u: ["F"],
    i: ["O"],
    o: ["U"],
    p: ["J"],
    open_bracket: [";", ":"],
    close_bracket: ["=", "+"],
    backslash: ["\\", "|"],
    caps_lock: ["caps"],
    a: ["N"],
    s: ["R"],
    d: ["T"],
    f: ["S"],
    g: ["G"],
    h: ["Y"],
    j: ["H"],
    k: ["A"],
    l: ["E"],
    semicolon: ["I"],
    quote: [",", "?"],
    enter: ["return"],
    left_shift: ["shift"],
    z: ["Q"],
    x: ["X"],
    c: ["M"],
    v: ["C"],
    b: ["V"],
    n: ["K"],
    m: ["P"],
    comma: [".", ">"],
    period: ["-", "\""],
    slash: ["/", "<"],
    right_shift: ["shift"],
  }
}

module Keyboard = {
  type t = [
    | #Ansi(Ansi.t)
  ]

  @get external body: Dom.document => Dom.element = "body"

  let useKey = key => {
    open Webapi.Dom
    React.useEffect1(() => {
      let onKeyDown = (ev: KeyboardEvent.t) => {
        let code = KeyboardEvent.code(ev)
        let target = KeyboardEvent.target(ev)
        let body = document->body
        if String.equal(code, key) && Obj.magic(target) === body {
          ev->KeyboardEvent.preventDefault
          switch document->Document.getElementById(key) {
          | None => ()
          | Some(element) =>
            Js.log2("down", key)
            let css = {
              "color": ["rgb(15 23 42)", "rgb(241 245 249)"],
              "backgroundColor": ["rgb(241 245 249)", "rgb(15 23 42)"],
            }
            let timing = {
              "duration": 1000,
              "iterations": 1,
              "easing": "ease",
            }
            Js.log(element->Element.animate(css, timing))
          }
        } else {
          Js.log(code)
        }
      }
      window->Window.addKeyDownEventListener(onKeyDown)
      Some(() => window->Window.removeKeyDownEventListener(onKeyDown))
    }, [key])
  }

  let keyIds = [
    "Escape",
    "F1",
    "F2",
    "F3",
    "F4",
    "F5",
    "F6",
    "F7",
    "F8",
    "F9",
    "F10",
    "F11",
    "F12",
    "Tab",
    "Backquote",
    "Digit1",
    "Digit2",
    "Digit3",
    "Digit4",
    "Digit5",
    "Digit6",
    "Digit7",
    "Digit8",
    "Digit9",
    "Digit0",
    "Minus",
    "Equal",
    "Backspace",
    "KeyQ",
    "KeyW",
    "KeyE",
    "KeyR",
    "KeyT",
    "KeyY",
    "KeyU",
    "KeyI",
    "KeyO",
    "KeyP",
    "BracketLeft",
    "BracketRight",
    "Backslash",
    "ControlRight",
    "KeyA",
    "KeyS",
    "KeyD",
    "KeyF",
    "KeyG",
    "KeyH",
    "KeyJ",
    "KeyK",
    "KeyL",
    "Semicolon",
    "Quote",
    "Enter",
    "ShiftLeft",
    "KeyZ",
    "KeyX",
    "KeyC",
    "KeyV",
    "KeyB",
    "KeyN",
    "KeyM",
    "Comma",
    "Period",
    "Slash",
    "ShiftRight",
    "ControlLeft",
    "AltLeft",
    "OSLeft",
    "Space",
    "OSRight",
    "AltRight",
  ]

  @react.component
  let make = (~t: t) => {
    keyIds->Belt.Array.forEach(useKey)
    switch t {
    | #Ansi({
        tilde,
        one,
        two,
        three,
        four,
        five,
        six,
        seven,
        eight,
        nine,
        zero,
        hyphen,
        equal,
        backspace,
        tab,
        q,
        w,
        e,
        r,
        t,
        y,
        u,
        i,
        o,
        p,
        open_bracket,
        close_bracket,
        backslash,
        caps_lock,
        a,
        s,
        d,
        f,
        g,
        h,
        j,
        k,
        l,
        semicolon,
        quote,
        enter,
        left_shift,
        z,
        x,
        c,
        v,
        b,
        n,
        m,
        comma,
        period,
        slash,
        right_shift,
      }) =>
      <div className="p-[6px] bg-slate-700 rounded-2xl shadow-lg shadow-black">
        <div className="flex flex-col">
          <div className="flex flex-row">
            <Key id="Escape" label={["esc"]} shape={#_1_5} />
            <Key id="F1" label={["F1"]} shape={#_1} />
            <Key id="F2" label={["F2"]} shape={#_1} />
            <Key id="F3" label={["F3"]} shape={#_1} />
            <Key id="F4" label={["F4"]} shape={#_1} />
            <Key id="F5" label={["F5"]} shape={#_1} />
            <Key id="F6" label={["F6"]} shape={#_1} />
            <Key id="F7" label={["F7"]} shape={#_1} />
            <Key id="F8" label={["F8"]} shape={#_1} />
            <Key id="F9" label={["F9"]} shape={#_1} />
            <Key id="F10" label={["F10"]} shape={#_1} />
            <Key id="F11" label={["F11"]} shape={#_1} />
            <Key id="F12" label={["F12"]} shape={#_1} />
            <Key label={[""]} shape={#_1} />
          </div>
          <div className="flex flex-row">
            <Key id="Backquote" label={tilde} shape={#_1} />
            <Key id="Digit1" label={one} shape={#_1} />
            <Key id="Digit2" label={two} shape={#_1} />
            <Key id="Digit3" label={three} shape={#_1} />
            <Key id="Digit4" label={four} shape={#_1} />
            <Key id="Digit5" label={five} shape={#_1} />
            <Key id="Digit6" label={six} shape={#_1} />
            <Key id="Digit7" label={seven} shape={#_1} />
            <Key id="Digit8" label={eight} shape={#_1} />
            <Key id="Digit9" label={nine} shape={#_1} />
            <Key id="Digit0" label={zero} shape={#_1} />
            <Key id="Minus" label={hyphen} shape={#_1} />
            <Key id="Equal" label={equal} shape={#_1} />
            <Key id="Backspace" label={backspace} shape={#_1_5} />
          </div>
          <div className="flex flex-row">
            <Key id="Tab" label={tab} shape={#_1_5} />
            <Key id="KeyQ" label={q} shape={#_1} />
            <Key id="KeyW" label={w} shape={#_1} />
            <Key id="KeyE" label={e} shape={#_1} />
            <Key id="KeyR" label={r} shape={#_1} />
            <Key id="KeyT" label={t} shape={#_1} />
            <Key id="KeyY" label={y} shape={#_1} />
            <Key id="KeyU" label={u} shape={#_1} />
            <Key id="KeyI" label={i} shape={#_1} />
            <Key id="KeyO" label={o} shape={#_1} />
            <Key id="KeyP" label={p} shape={#_1} />
            <Key id="BracketLeft" label={open_bracket} shape={#_1} />
            <Key id="BracketRight" label={close_bracket} shape={#_1} />
            <Key id="Backslash" label={backslash} shape={#_1} />
          </div>
          <div className="flex flex-row">
            <Key id="ControlRight" label={caps_lock} shape={#_1_75} />
            <Key id="KeyA" label={a} shape={#_1} />
            <Key id="KeyS" label={s} shape={#_1} />
            <Key id="KeyD" label={d} shape={#_1} />
            <Key id="KeyF" label={f} shape={#_1} />
            <Key id="KeyG" label={g} shape={#_1} />
            <Key id="KeyH" label={h} shape={#_1} />
            <Key id="KeyJ" label={j} shape={#_1} />
            <Key id="KeyK" label={k} shape={#_1} />
            <Key id="KeyL" label={l} shape={#_1} />
            <Key id="Semicolon" label={semicolon} shape={#_1} />
            <Key id="Quote" label={quote} shape={#_1} />
            <Key id="Enter" label={enter} shape={#_1_75} />
          </div>
          <div className="flex flex-row">
            <Key id="ShiftLeft" label={left_shift} shape={#_2_25} />
            <Key id="KeyZ" label={z} shape={#_1} />
            <Key id="KeyX" label={x} shape={#_1} />
            <Key id="KeyC" label={c} shape={#_1} />
            <Key id="KeyV" label={v} shape={#_1} />
            <Key id="KeyB" label={b} shape={#_1} />
            <Key id="KeyN" label={n} shape={#_1} />
            <Key id="KeyM" label={m} shape={#_1} />
            <Key id="Comma" label={comma} shape={#_1} />
            <Key id="Period" label={period} shape={#_1} />
            <Key id="Slash" label={slash} shape={#_1} />
            <Key id="ShiftRight" label={right_shift} shape={#_2_25} />
          </div>
          <div className="flex flex-row">
            <Key label={["fn"]} shape={#_1} />
            <Key id="ControlLeft" label={["^"]} shape={#_1} />
            <Key id="AltLeft" label={["⌥"]} shape={#_1} />
            <Key id="OSLeft" label={["⌘"]} shape={#_1_25} />
            <Key id="Space" label={[""]} shape={#_5} />
            <Key id="OSRight" label={["⌘"]} shape={#_1_25} />
            <Key id="AltRight" label={["⌥"]} shape={#_1} />
            <Key label={["<^v>"]} shape={#Nav} />
          </div>
        </div>
      </div>
    }
  }
}

type props = {content: string}

let getStaticProps: Next.GetStaticProps.t<props, 'params, 'previewData> = _context => {
  let props = {content: "Hello World"}
  Js.Promise.resolve({"props": props})
}

let default = (_props: props) => {
  <div className="flex flex-col gap-8">
    <Keyboard t={#Ansi(Ansi.graphite)} />
    <Keyboard t={#Ansi(Ansi.qwerty)} />
  </div>
}
