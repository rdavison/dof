type quote = {
  text: string,
  source: string,
  length: int,
  id: int,
}

type corpus = {
  language: string,
  groups: array<array<int>>,
  quotes: array<quote>,
}

@scope("JSON") @val external parseCorpus: string => corpus = "parse"

// module Game = {
//   let useViewport = (rows, cols, data) => {
//     let (pos, setPos) = React.useState(() => 0)
//     let len = String.length(data)
//     let left = () => setPos(pos => pos > 0 ? pos + 1 : 0)
//     let right = () => setPos(pos => pos < len - 1 ? pos + 1 : len - 1)
//     let arr = ""->Array.make(~length=rows * cols)
//     let wordBoundary = {
//       let posRef = ref(pos)
//       while posRef.contents < 0 && data->String.charAt(posRef.contents) != " " {
//         decr(posRef)
//       }
//       posRef.contents
//     }

//   }

//   @react.component
//   let make = (~corpus) => {
//     let quotes = corpus.quotes
//     let quote = quotes->Array.getUnsafe(120)
//     let text = quote.text->String.split(" ")->Array.flatMap(word => word->String.split(""))
//     let viewport =
//       <table className="w-[942px] px-4 max-h-[300px] overflow-hidden m-auto text-6xl font-mono">
//         {text
//         ->Array.mapWithIndex((word, i) => {
//           let iStr = Int.toString(i)
//           <tr key={`w-${iStr}`} className="">
//             {word
//             ->Array.mapWithIndex((char, j) => {
//               let jStr = Int.toString(j)
//               <td key={`w-${iStr}-${jStr}`} className=""> {React.string(char)} </td>
//             })
//             ->React.array}
//           </tr>
//         })
//         ->React.array}
//       </table>
//   }
// }

module Key = {
  type shape = [
    | #_1
    | #_1_25
    | #_1_5
    | #_1_75
    | #_2
    | #_2_25
    | #_5
  ]

  module Power = {
    @react.component
    let make = () => {
      let shape = "h-[60px] w-[60px]"
      <div
        className={`${shape} m-[2px] bg-slate-900 hover:bg-slate-100 transition-colors rounded-lg shadow-2xl shadow-slate-700 text-white ease-out hover:duration-75 duration-[2000ms] hover:text-slate-900 items-center flex`}>
        <div className="text-4xl m-auto text-slate-800"> {React.string("○")} </div>
      </div>
    }
  }

  module Nav = {
    @react.component
    let make = () => {
      <div className="h-[60px] w-[188px] grid gap-0 grid-cols-3 grid-rows-2">
        <div />
        <div
          id="ArrowUp"
          className={`text-xs my-[2px] bg-slate-900 hover:bg-slate-100 transition-colors rounded-lg shadow-2xl shadow-slate-700 text-white ease-out hover:duration-75 duration-[2000ms] hover:text-slate-900 items-center flex`}>
          <div className="m-auto"> {React.string("▲")} </div>
        </div>
        <div />
        <div
          id="ArrowLeft"
          className={`text-xs mt-[2px] mr-[2px] bg-slate-900 hover:bg-slate-100 transition-colors rounded-lg shadow-2xl shadow-slate-700 text-white ease-out hover:duration-75 duration-[2000ms] hover:text-slate-900 items-center flex`}>
          <div className="m-auto"> {React.string("◀")} </div>
        </div>
        <div
          id="ArrowDown"
          className={`text-xs mt-[2px] bg-slate-900 hover:bg-slate-100 transition-colors rounded-lg shadow-2xl shadow-slate-700 text-white ease-out hover:duration-75 duration-[2000ms] hover:text-slate-900 items-center flex`}>
          <div className="m-auto"> {React.string("▼")} </div>
        </div>
        <div
          id="ArrowRight"
          className={`text-xs mt-[2px] ml-[2px] bg-slate-900 hover:bg-slate-100 transition-colors rounded-lg shadow-2xl shadow-slate-700 text-white ease-out hover:duration-75 duration-[2000ms] hover:text-slate-900 items-center flex`}>
          <div className="m-auto"> {React.string("▶")} </div>
        </div>
      </div>
    }
  }

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
    // In the layout editor, character keys can be dragged and selected.
    let edit = switch React.useContext(Editor.context) {
    | Some(e) if e.editable(id) => Some(e)
    | _ => None
    }
    let editing = switch edit {
    | Some(e) =>
      " cursor-grab select-none touch-none" ++ if e.target == Some(id) {
        " ring-2 ring-amber-300"
      } else if e.selected == Some(id) {
        " ring-2 ring-sky-400"
      } else {
        ""
      } ++ (e.source == Some(id) ? " opacity-30" : "")
    | None => ""
    }
    let key =
      <div
        id
        className={`${shape} m-[2px] bg-slate-900 hover:bg-slate-100 transition-colors rounded-lg shadow-2xl shadow-slate-700 text-white ease-out hover:duration-75 duration-[2000ms] hover:text-slate-900 items-center flex${editing}`}
        onPointerDown=?{edit->Option.map(e => ev => e.onPointerDown(id, ev))}>
        <div className="m-auto"> {label} </div>
      </div>
    switch edit {
    | Some(_) => React.cloneElement(key, {"data-key": id})
    | None => key
    }
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

  let amaranth = {
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
    q: ["Q"],
    w: ["Y"],
    e: ["O"],
    r: ["U"],
    t: ["J"],
    y: ["K"],
    u: ["D"],
    i: ["L"],
    o: ["C"],
    p: ["W"],
    open_bracket: ["X"],
    close_bracket: ["=", "+"],
    backslash: ["\\", "|"],
    caps_lock: ["caps"],
    a: ["A"],
    s: ["I"],
    d: ["E"],
    f: ["N"],
    g: ["P"],
    h: ["M"],
    j: ["H"],
    k: ["T"],
    l: ["S"],
    semicolon: ["R"],
    quote: ["-", "_"],
    enter: ["return"],
    left_shift: ["shift"],
    z: ["/", "?"],
    x: [";", "<"],
    c: [",", ">"],
    v: [".", ":"],
    b: ["'", "\""],
    n: ["B"],
    m: ["F"],
    comma: ["G"],
    period: ["V"],
    slash: ["Z"],
    right_shift: ["shift"],
  }

  let dvorak = {
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
    q: ["'", "\""],
    w: [",", "<"],
    e: [".", ">"],
    r: ["P"],
    t: ["Y"],
    y: ["F"],
    u: ["G"],
    i: ["C"],
    o: ["R"],
    p: ["L"],
    open_bracket: ["/", "?"],
    close_bracket: ["=", "+"],
    backslash: ["\\", "|"],
    caps_lock: ["caps"],
    a: ["A"],
    s: ["O"],
    d: ["E"],
    f: ["U"],
    g: ["I"],
    h: ["D"],
    j: ["H"],
    k: ["T"],
    l: ["N"],
    semicolon: ["S"],
    quote: ["-", "_"],
    enter: ["return"],
    left_shift: ["shift"],
    z: [";", ":"],
    x: ["Q"],
    c: ["J"],
    v: ["K"],
    b: ["X"],
    n: ["B"],
    m: ["M"],
    comma: ["W"],
    period: ["V"],
    slash: ["Z"],
    right_shift: ["shift"],
  }

  let colemak = {
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
    e: ["F"],
    r: ["P"],
    t: ["G"],
    y: ["J"],
    u: ["L"],
    i: ["U"],
    o: ["Y"],
    p: [";", ":"],
    open_bracket: ["[", "{"],
    close_bracket: ["]", "}"],
    backslash: ["\\", "|"],
    caps_lock: ["caps"],
    a: ["A"],
    s: ["R"],
    d: ["S"],
    f: ["T"],
    g: ["D"],
    h: ["H"],
    j: ["N"],
    k: ["E"],
    l: ["I"],
    semicolon: ["O"],
    quote: ["'", "\""],
    enter: ["return"],
    left_shift: ["shift"],
    z: ["Z"],
    x: ["X"],
    c: ["C"],
    v: ["V"],
    b: ["B"],
    n: ["K"],
    m: ["M"],
    comma: [",", "<"],
    period: [".", ">"],
    slash: ["/", "?"],
    right_shift: ["shift"],
  }

  // The layouts the board can switch between, in the order they are offered.
  type layout = {id: string, name: string, board: t}
  let layouts = [
    {id: "amaranth", name: "Amaranth", board: amaranth},
    {id: "qwerty", name: "QWERTY", board: qwerty},
    {id: "dvorak", name: "Dvorak", board: dvorak},
    {id: "colemak", name: "Colemak", board: colemak},
  ]

  // Every character-bearing physical key, named by its KeyboardEvent code and
  // paired with the labels a layout puts on it. The modifiers are left out:
  // they carry no characters, so nothing can be typed on them.
  let keycodes = (t: t) => [
    ("Backquote", t.tilde),
    ("Digit1", t.one),
    ("Digit2", t.two),
    ("Digit3", t.three),
    ("Digit4", t.four),
    ("Digit5", t.five),
    ("Digit6", t.six),
    ("Digit7", t.seven),
    ("Digit8", t.eight),
    ("Digit9", t.nine),
    ("Digit0", t.zero),
    ("Minus", t.hyphen),
    ("Equal", t.equal),
    ("KeyQ", t.q),
    ("KeyW", t.w),
    ("KeyE", t.e),
    ("KeyR", t.r),
    ("KeyT", t.t),
    ("KeyY", t.y),
    ("KeyU", t.u),
    ("KeyI", t.i),
    ("KeyO", t.o),
    ("KeyP", t.p),
    ("BracketLeft", t.open_bracket),
    ("BracketRight", t.close_bracket),
    ("Backslash", t.backslash),
    ("KeyA", t.a),
    ("KeyS", t.s),
    ("KeyD", t.d),
    ("KeyF", t.f),
    ("KeyG", t.g),
    ("KeyH", t.h),
    ("KeyJ", t.j),
    ("KeyK", t.k),
    ("KeyL", t.l),
    ("Semicolon", t.semicolon),
    ("Quote", t.quote),
    ("KeyZ", t.z),
    ("KeyX", t.x),
    ("KeyC", t.c),
    ("KeyV", t.v),
    ("KeyB", t.b),
    ("KeyN", t.n),
    ("KeyM", t.m),
    ("Comma", t.comma),
    ("Period", t.period),
    ("Slash", t.slash),
  ]

  // The character keys, by code, in board order.
  let codes = keycodes(qwerty)->Array.map(fst)

  let labelsOf = (t, code) =>
    keycodes(t)->Array.find(((c, _)) => c == code)->Option.map(snd)

  // `t` with one key's labels replaced. A code that is not a character key
  // leaves it as it is.
  let withKey = (t, code, labels) =>
    switch code {
    | "Backquote" => {...t, tilde: labels}
    | "Digit1" => {...t, one: labels}
    | "Digit2" => {...t, two: labels}
    | "Digit3" => {...t, three: labels}
    | "Digit4" => {...t, four: labels}
    | "Digit5" => {...t, five: labels}
    | "Digit6" => {...t, six: labels}
    | "Digit7" => {...t, seven: labels}
    | "Digit8" => {...t, eight: labels}
    | "Digit9" => {...t, nine: labels}
    | "Digit0" => {...t, zero: labels}
    | "Minus" => {...t, hyphen: labels}
    | "Equal" => {...t, equal: labels}
    | "KeyQ" => {...t, q: labels}
    | "KeyW" => {...t, w: labels}
    | "KeyE" => {...t, e: labels}
    | "KeyR" => {...t, r: labels}
    | "KeyT" => {...t, t: labels}
    | "KeyY" => {...t, y: labels}
    | "KeyU" => {...t, u: labels}
    | "KeyI" => {...t, i: labels}
    | "KeyO" => {...t, o: labels}
    | "KeyP" => {...t, p: labels}
    | "BracketLeft" => {...t, open_bracket: labels}
    | "BracketRight" => {...t, close_bracket: labels}
    | "Backslash" => {...t, backslash: labels}
    | "KeyA" => {...t, a: labels}
    | "KeyS" => {...t, s: labels}
    | "KeyD" => {...t, d: labels}
    | "KeyF" => {...t, f: labels}
    | "KeyG" => {...t, g: labels}
    | "KeyH" => {...t, h: labels}
    | "KeyJ" => {...t, j: labels}
    | "KeyK" => {...t, k: labels}
    | "KeyL" => {...t, l: labels}
    | "Semicolon" => {...t, semicolon: labels}
    | "Quote" => {...t, quote: labels}
    | "KeyZ" => {...t, z: labels}
    | "KeyX" => {...t, x: labels}
    | "KeyC" => {...t, c: labels}
    | "KeyV" => {...t, v: labels}
    | "KeyB" => {...t, b: labels}
    | "KeyN" => {...t, n: labels}
    | "KeyM" => {...t, m: labels}
    | "Comma" => {...t, comma: labels}
    | "Period" => {...t, period: labels}
    | "Slash" => {...t, slash: labels}
    | _ => t
    }

  let swap = (t, a, b) =>
    switch (labelsOf(t, a), labelsOf(t, b)) {
    | (Some(la), Some(lb)) => t->withKey(a, lb)->withKey(b, la)
    | _ => t
    }

  // What a key types without and with shift. A single label is a letter, whose
  // base is its lower case.
  let layers = labels =>
    switch labels {
    | [x] => (x->String.toLowerCase, x)
    | [x, y] => (x, y)
    | _ => ("", "")
    }

  // The labels for a key typing `base` and `shift`. A letter with its own
  // capital on shift keeps the single-label look.
  let labelsFor = (base, shift) =>
    base != shift && base->String.toLowerCase == base && base->String.toUpperCase == shift
      ? [shift]
      : [base, shift]

  // Counts characters as people see them, so an emoji is one.
  let length: string => int = %raw(`s => [...s].length`)

  // A layout as a file, for saving, export and import.
  let toJson = (name, t) => {
    let keys = Js.Dict.empty()
    keycodes(t)->Array.forEach(((code, labels)) => {
      let (base, shift) = layers(labels)
      keys->Js.Dict.set(
        code,
        Js.Json.object_(
          Js.Dict.fromArray([("base", Js.Json.string(base)), ("shift", Js.Json.string(shift))]),
        ),
      )
    })
    Js.Json.object_(
      Js.Dict.fromArray([
        ("format", Js.Json.string("dof-layout")),
        ("version", Js.Json.number(1.)),
        ("name", Js.Json.string(name)),
        ("keys", Js.Json.object_(keys)),
      ]),
    )
  }

  let stringify = (name, t) => toJson(name, t)->Js.Json.stringifyWithSpace(2)

  exception Invalid(string)

  // A layout file read back. Keys it leaves out come from QWERTY. The error
  // names what is wrong, for showing to whoever pasted it.
  let fromJson = json =>
    try {
      let obj = switch json->Js.Json.decodeObject {
      | Some(o) => o
      | None => raise(Invalid("not a layout: expected an object"))
      }
      let get = (o, k) => o->Js.Dict.get(k)
      if get(obj, "format")->Option.flatMap(Js.Json.decodeString) != Some("dof-layout") {
        raise(Invalid(`not a layout: "format" should be "dof-layout"`))
      }
      if get(obj, "version")->Option.flatMap(Js.Json.decodeNumber) != Some(1.) {
        raise(Invalid(`unsupported "version": expected 1`))
      }
      let name = switch get(obj, "name")->Option.flatMap(Js.Json.decodeString) {
      | Some(n) if n->String.trim != "" => n->String.trim
      | _ => raise(Invalid(`"name" must be a non-empty string`))
      }
      let keys = switch get(obj, "keys")->Option.flatMap(Js.Json.decodeObject) {
      | Some(k) => k
      | None => raise(Invalid(`"keys" must be an object`))
      }
      let board = keys->Js.Dict.entries->Array.reduce(qwerty, (t, (code, v)) => {
        if !(codes->Array.includes(code)) {
          raise(Invalid(`${code} is not a character key`))
        }
        let o = switch v->Js.Json.decodeObject {
        | Some(o) => o
        | None => raise(Invalid(`${code}: expected {"base": …, "shift": …}`))
        }
        let one = layer =>
          switch get(o, layer)->Option.flatMap(Js.Json.decodeString) {
          | Some(c) if length(c) == 1 => c
          | _ => raise(Invalid(`${code}: ${layer} must be one character`))
          }
        t->withKey(code, labelsFor(one("base"), one("shift")))
      })
      Ok((name, board))
    } catch {
    | Invalid(message) => Error(message)
    }

  let parse = text =>
    switch try {
      Some(Js.Json.parseExn(text))
    } catch {
    | _ => None
    } {
    | Some(json) => fromJson(json)
    | None => Error("not valid JSON")
    }

  // Note: this is Graphite proper, not the `amaranth` record above, and does not
  // agree with it. Only `runText` still reads it; the live keyboard goes through the
  // record, so derive from `keycodes` rather than from here.
  let graphite2Keycode = char =>
    switch char {
    | "`" | "~" => "Backquote"
    | "1" | "!" => "Digit1"
    | "2" | "@" => "Digit2"
    | "3" | "#" => "Digit3"
    | "4" | "$" => "Digit4"
    | "5" | "%" => "Digit5"
    | "6" | "^" => "Digit6"
    | "7" | "&" => "Digit7"
    | "8" | "*" => "Digit8"
    | "9" | "(" => "Digit9"
    | "0" | ")" => "Digit0"
    | "[" | "{" => "Minus"
    | "]" | "}" => "Equal"
    | "b" | "B" => "KeyQ"
    | "l" | "L" => "KeyW"
    | "d" | "D" => "KeyE"
    | "w" | "W" => "KeyR"
    | "z" | "Z" => "KeyT"
    | "'" | "_" => "KeyY"
    | "f" | "F" => "KeyU"
    | "o" | "O" => "KeyI"
    | "u" | "U" => "KeyO"
    | "j" | "J" => "KeyP"
    | ";" | ":" => "BracketLeft"
    | "=" | "+" => "BracketRight"
    | "n" | "N" => "KeyA"
    | "r" | "R" => "KeyS"
    | "t" | "T" => "KeyD"
    | "s" | "S" => "KeyF"
    | "g" | "G" => "KeyG"
    | "y" | "Y" => "KeyH"
    | "h" | "H" => "KeyJ"
    | "a" | "A" => "KeyK"
    | "e" | "E" => "KeyL"
    | "i" | "I" => "Semicolon"
    | "," | "?" => "Quote"
    | "q" | "Q" => "KeyZ"
    | "x" | "X" => "KeyX"
    | "m" | "M" => "KeyC"
    | "c" | "C" => "KeyV"
    | "v" | "V" => "KeyB"
    | "k" | "K" => "KeyN"
    | "p" | "P" => "KeyM"
    | "." | ">" => "Comma"
    | "-" | "\"" => "Period"
    | "/" | "<" => "Slash"
    | " " => "Space"
    | _ => ""
    }
}

let c = c =>
  switch c {
  | #white => "rgb(241 245 249)"
  | #indigo => "rgb(99 102 241)"
  | #green => "rgb(34 197 94)"
  | #blue => "rgb(59 130 246)"
  | #yellow1 => "rgb(253 224 71)"
  | #yellow2 => "rgb(254 240 138)"
  | #black => "rgb(15 23 42)"
  | #red => "rgb(239 68 68)"
  }

let sim = (keycode, color) => {
  switch document->Webapi.Dom.Document.getElementById(keycode) {
  | None => ()
  | Some(element) =>
    // Js.log2("down", keycode)
    let css = {
      "color": switch color {
      | #indigo => [c(#white), c(#white)]
      | #red => [c(#white), c(#white)]
      | #green => [c(#white), c(#white)]
      | #blue => [c(#white), c(#white)]
      | #yellow1 => [c(#black), c(#white)]
      | #white => [c(#black), c(#white)]
      },
      "backgroundColor": switch color {
      | #indigo => [c(#indigo), c(#black)]
      | #red => [c(#red), c(#black)]
      | #green => [c(#green), c(#black)]
      | #blue => [c(#blue), c(#black)]
      | #yellow1 => [c(#yellow1), c(#black)]
      | #white => [c(#white), c(#black)]
      },
    }
    let timing = {
      "duration": 1000,
      "iterations": 1,
      "easing": "ease",
    }
    let _ = element->Webapi.Dom.Element.animate(css, timing)
  }
}

let delay = timeout => {
  Promise.make((resolve, _reject) => {
    let _ = setTimeout(() => resolve(. ()), timeout)
  })
}

let runKeycode = (keycode, color) => {
  sim(keycode, color)
  switch keycode {
  | "Space" => delay(200)
  | _ => delay(50)
  }
}

let kcHF = keycode => {
  switch keycode {
  | "Escape" => (#l, #r)
  | "F1" => (#l, #r)
  | "F2" => (#l, #m)
  | "F3" => (#l, #m)
  | "F4" => (#l, #i)
  | "F5" => (#l, #i)
  | "F6" => (#r, #i)
  | "F7" => (#r, #i)
  | "F8" => (#r, #m)
  | "F9" => (#r, #r)
  | "F10" => (#r, #i)
  | "F11" => (#r, #m)
  | "F12" => (#r, #r)
  | "Tab" => (#l, #p)
  | "Backquote" => (#l, #r)
  | "Digit1" => (#l, #r)
  | "Digit2" => (#l, #r)
  | "Digit3" => (#l, #m)
  | "Digit4" => (#l, #i)
  | "Digit5" => (#l, #i)
  | "Digit6" => (#r, #i)
  | "Digit7" => (#r, #i)
  | "Digit8" => (#r, #m)
  | "Digit9" => (#r, #m)
  | "Digit0" => (#r, #p)
  | "Minus" => (#r, #m)
  | "Equal" => (#r, #r)
  | "Backspace" => (#r, #r)
  | "KeyQ" => (#l, #p)
  | "KeyW" => (#l, #r)
  | "KeyE" => (#l, #m)
  | "KeyR" => (#l, #i)
  | "KeyT" => (#l, #i)
  | "KeyY" => (#r, #i)
  | "KeyU" => (#r, #i)
  | "KeyI" => (#r, #m)
  | "KeyO" => (#r, #r)
  | "KeyP" => (#r, #p)
  | "BracketLeft" => (#r, #p)
  | "BracketRight" => (#r, #p)
  | "Backslash" => (#r, #p)
  | "ControlRight" => (#l, #p)
  | "KeyA" => (#l, #p)
  | "KeyS" => (#l, #r)
  | "KeyD" => (#l, #m)
  | "KeyF" => (#l, #i)
  | "KeyG" => (#l, #i)
  | "KeyH" => (#r, #i)
  | "KeyJ" => (#r, #i)
  | "KeyK" => (#r, #m)
  | "KeyL" => (#r, #r)
  | "Semicolon" => (#r, #p)
  | "Quote" => (#r, #p)
  | "Enter" => (#r, #p)
  | "ShiftLeft" => (#l, #p)
  | "KeyZ" => (#l, #p)
  | "KeyX" => (#l, #r)
  | "KeyC" => (#l, #m)
  | "KeyV" => (#l, #i)
  | "KeyB" => (#l, #i)
  | "KeyN" => (#r, #i)
  | "KeyM" => (#r, #i)
  | "Comma" => (#r, #m)
  | "Period" => (#r, #r)
  | "Slash" => (#r, #p)
  | "ShiftRight" => (#r, #p)
  | "ControlLeft" => (#l, #p)
  | "AltLeft" => (#l, #t)
  | "OSLeft" => (#l, #t)
  | "Space" => (#r, #t)
  | "OSRight" => (#r, #t)
  | "AltRight" => (#r, #t)
  | "ArrowUp" => (#r, #m)
  | "ArrowLeft" => (#r, #i)
  | "ArrowDown" => (#r, #m)
  | "ArrowRight" => (#r, #r)
  | _ => failwith("Invalid")
  }
}

// Which finger presses each key. A fingermap profile starts from every key's
// default finger (`kcHF`); the stronglytyped profile adds the alternate
// fingerings recorded on /alt-fingerings, keyed by the physical key run of a
// whole word. Where an entry exists it overrides the default finger for every
// key in the word, so `judge` is fingered 4 3 7 8 3 rather than resting both
// j and u on the left index.
module Fingering = {
  type entry = {
    keys: array<string>,
    status: string,
    fingers: array<string>,
  }

  let code = hf =>
    switch hf {
    | (#l, #p) => "lp"
    | (#l, #r) => "lr"
    | (#l, #m) => "lm"
    | (#l, #i) => "li"
    | (#l, #t) => "lt"
    | (#r, #i) => "ri"
    | (#r, #m) => "rm"
    | (#r, #r) => "rr"
    | (#r, #p) => "rp"
    | (#r, #t) => "rt"
    }

  let sigOf = keys => keys->Array.joinWith(" ")

  // Which physical key carries a character, read off the record that draws the
  // board. Ansi.graphite2Keycode is Graphite proper, a different layout.
  let keyMap = board => {
    let d = Dict.make()
    Ansi.keycodes(board)->Array.forEach(((keycode, labels)) =>
      labels->Array.forEach(label => {
        let ch = label->String.toLowerCase
        switch d->Dict.get(ch) {
        | Some(_) => () // a character on two keys: the first one wins
        | None => d->Dict.set(ch, keycode)
        }
      })
    )
    d
  }

  // The map for the layout on the board.
  let charKey = ref(keyMap(Ansi.amaranth))

  let setLayout = board => charKey := keyMap(board)

  // Only "alt" entries carry a fingering; "none" means the default is the only
  // way to type the word, which is what a lookup miss already yields.
  let table: ref<Dict.t<array<string>>> = ref(Dict.make())

  let install = entries => {
    let d = Dict.make()
    entries->Array.forEach(e =>
      if e.status == "alt" && e.fingers->Array.length == e.keys->Array.length {
        d->Dict.set(sigOf(e.keys), e.fingers)
      }
    )
    table := d
  }

  let defaults = keys => keys->Array.map(k => code(kcHF(k)))

  type profile = {
    id: string,
    name: string,
    // whether the recorded alternate fingerings apply
    alternates: bool,
  }

  let profiles = [
    {id: "stronglytyped", name: "stronglytyped", alternates: true},
    {id: "standard", name: "standard", alternates: false},
  ]

  // The profile in use.
  let profile = ref(profiles->Array.getUnsafe(0))

  let setProfile = id => profiles->Array.find(p => p.id == id)->Option.forEach(p => profile := p)

  let forKeys = keys =>
    switch profile.contents.alternates ? table.contents->Dict.get(sigOf(keys)) : None {
    | Some(fingers) => fingers
    | None => defaults(keys)
    }

  // The word surrounding `offset` in `text`, as the review page tokenizes it:
  // a run of [a-z'-] with leading and trailing apostrophes and hyphens dropped.
  let isWordChar = ch => (ch >= "a" && ch <= "z") || ch == "'" || ch == "-"
  let isEdge = ch => ch == "'" || ch == "-"

  let charAt = (text, i) =>
    i < 0 || i >= text->String.length ? "" : text->Js.String2.charAt(i)->String.toLowerCase

  let wordAround = (text, offset) => {
    if !isWordChar(charAt(text, offset)) {
      None
    } else {
      let start = ref(offset)
      while isWordChar(charAt(text, start.contents - 1)) {
        start := start.contents - 1
      }
      let stop = ref(offset)
      while isWordChar(charAt(text, stop.contents + 1)) {
        stop := stop.contents + 1
      }
      while start.contents < stop.contents && isEdge(charAt(text, start.contents)) {
        start := start.contents + 1
      }
      while stop.contents > start.contents && isEdge(charAt(text, stop.contents)) {
        stop := stop.contents - 1
      }
      if offset < start.contents || offset > stop.contents {
        None
      } else {
        let keys = []
        for i in start.contents to stop.contents {
          switch charKey.contents->Dict.get(charAt(text, i)) {
          | Some(keycode) => keys->Array.push(keycode)
          | None => ()
          }
        }
        keys->Array.length == stop.contents - start.contents + 1
          ? Some(keys, offset - start.contents)
          : None
      }
    }
  }

  // The finger that actually presses the key at `offset`, consulting the
  // recorded fingering for the word it sits in.
  let at = (text, offset, keycode) =>
    switch wordAround(text, offset) {
    | Some(keys, i) =>
      switch forKeys(keys)->Array.get(i) {
      | Some(f) => f
      | None => code(kcHF(keycode))
      }
    | None => code(kcHF(keycode))
    }
}

// SVG geometry the bindings don't cover.
type svgElement = Dom.element
type svgPoint = {x: float, y: float}
type animation = Dom.animation
@send external getTotalLength: svgElement => float = "getTotalLength"
@send external getPointAtLength: (svgElement, float) => svgPoint = "getPointAtLength"
@send external cancelAnimation: animation => unit = "cancel"

// Per-word, per-hand orb trails.
//
// When a word comes up, each hand's keys in it are planned as one hidden path:
// a point for one key, a pill for two, and for three or more a smooth curve
// through every key in order. As the word is typed a glowing orb runs along
// each hand's path and the trail fills in behind it, so the lit part is how
// far through the word that hand has got.
//
// The trail fades whenever typing stops. Each keystroke in the word refills it
// to full and starts the fade over, but once it has faded out it is gone: the
// next key in the word starts a fresh trail from there.
//
// Same-finger events get a glass "bump" that swells and fades: a pill joining
// the two keys, for a same-finger bigram (two different keys pressed back to
// back by one finger) or skipgram (the same, one press apart, even across a
// space). A bigram's pill glows and its second key flashes; a skipgram's pill
// is a shadow sunk into the board.
module Trace = {
  module Dom = Webapi.Dom

  // Set false to hide trails so the same-finger bumps read on their own.
  let showTrails = true

  let fadeMs = 500
  let bumpMs = 450
  let traceMs = 150

  // The trail is a glowing comet: brightest and widest at the orb, thinning to
  // nothing some way back along the path. The tail reaches `tailReach` times the
  // last hop, so it always stretches back past the previous key, and never
  // less than `tailPx`. It is drawn as pieces about `piecePx` long so each can
  // carry its own alpha.
  let tailPx = 160.
  let tailReach = 1.3
  let piecePx = 6.
  // alpha and width at the head of the comet, falling to nothing and to
  // `tailWidth` at the end of the tail
  let headOpacity = 0.95
  let headWidth = 14.
  let tailWidth = 2.

  let isAlpha = keycode =>
    switch keycode {
    | "KeyQ"
    | "KeyW"
    | "KeyE"
    | "KeyR"
    | "KeyT"
    | "KeyY"
    | "KeyU"
    | "KeyI"
    | "KeyO"
    | "KeyP"
    | "BracketLeft"
    | "BracketRight"
    | "KeyA"
    | "KeyS"
    | "KeyD"
    | "KeyF"
    | "KeyG"
    | "KeyH"
    | "KeyJ"
    | "KeyK"
    | "KeyL"
    | "Semicolon"
    | "Quote"
    | "KeyZ"
    | "KeyX"
    | "KeyC"
    | "KeyV"
    | "KeyB"
    | "KeyN"
    | "KeyM"
    | "Comma"
    | "Period"
    | "Slash" => true
    | _ => false
    }

  type color = [#red | #yellow1 | #green | #blue | #indigo | #white]

  // the most recent keypresses of any kind, newest first, with the finger
  // that pressed each
  let history: ref<array<(string, string)>> = ref([])

  let center = (element, origin) => {
    let r = element->Dom.Element.getBoundingClientRect
    let o = origin->Dom.Element.getBoundingClientRect
    (
      Dom.DomRect.left(r) -. Dom.DomRect.left(o) +. Dom.DomRect.width(r) /. 2.,
      Dom.DomRect.top(r) -. Dom.DomRect.top(o) +. Dom.DomRect.height(r) /. 2.,
    )
  }

  let byId = id => document->Dom.Document.getElementById(id)

  let svg = tag => document->Dom.Document.createElementNS("http://www.w3.org/2000/svg", tag)

  let isSpace = ch => ch == "" || ch == " " || ch == "\n"

  // The word under the cursor, as (start, stop) offsets: the run of non-space
  // characters holding the next character to type, or, once a word is typed
  // out, the run holding the last one.
  let wordAt = (text, offset) => {
    let ch = i => Fingering.charAt(text, i)
    let around = i => {
      let start = ref(i)
      while !isSpace(ch(start.contents - 1)) {
        start := start.contents - 1
      }
      let stop = ref(i)
      while !isSpace(ch(stop.contents + 1)) {
        stop := stop.contents + 1
      }
      (start.contents, stop.contents)
    }
    if !isSpace(ch(offset + 1)) {
      Some(around(offset + 1))
    } else if !isSpace(ch(offset)) {
      Some(around(offset))
    } else {
      None
    }
  }

  // One hand's keys in a word, in typing order, with the offset each is typed
  // at.
  type plan = {keys: array<string>, offsets: array<int>}

  // Split text[start..stop] by hand, using the recorded fingering for the
  // word. A key pressed again straight away is the same stop on the path.
  let planWord = (text, start, stop) => {
    let left = {keys: [], offsets: []}
    let right = {keys: [], offsets: []}
    for i in start to stop {
      switch Fingering.charKey.contents->Dict.get(Fingering.charAt(text, i)) {
      | Some(keycode) =>
        let hand = Fingering.at(text, i, keycode)->Js.String2.charAt(0) == "l" ? left : right
        if hand.keys->Array.get(hand.keys->Array.length - 1) != Some(keycode) {
          hand.keys->Array.push(keycode)
          hand.offsets->Array.push(i)
        }
      | None => ()
      }
    }
    [left, right]
  }

  // Path segments through `points`: a Catmull-Rom spline written as cubic
  // Béziers, so the curve passes through every point. With two points the
  // controls fall on the line between them, which draws a straight pill.
  let segments = points => {
    let f = Float.toString
    let n = points->Array.length
    let p = i => points->Array.getUnsafe(Js.Math.max_int(0, Js.Math.min_int(n - 1, i)))
    Array.fromInitializer(~length=Js.Math.max_int(0, n - 1), i => {
      let (ax, ay) = p(i - 1)
      let (bx, by) = p(i)
      let (cx, cy) = p(i + 1)
      let (dx, dy) = p(i + 2)
      let c1x = bx +. (cx -. ax) /. 6.
      let c1y = by +. (cy -. ay) /. 6.
      let c2x = cx -. (dx -. bx) /. 6.
      let c2y = cy -. (dy -. by) /. 6.
      `C ${f(c1x)} ${f(c1y)} ${f(c2x)} ${f(c2y)} ${f(cx)} ${f(cy)}`
    })
  }

  // One hand's trail for the word on screen.
  type trail = {
    offsets: array<int>,
    // path length at each key; the path is None for a single key
    lengths: array<float>,
    path: option<svgElement>,
    // the drawn trail, as (piece, start, end) lengths along the path
    pieces: array<(svgElement, float, float)>,
    orb: svgElement,
    // keys reached so far, and how much of the path is drawn
    mutable reached: int,
    mutable drawn: float,
    // how far back the tail reaches
    mutable tail: float,
    mutable frame: option<Webapi.rafId>,
  }

  type word = {
    text: string,
    // the word's span, and the offset its trail was planned from
    start: int,
    stop: int,
    from: int,
    group: svgElement,
    trails: array<trail>,
    // the last cursor offset seen, and whether anything has been typed yet
    mutable seen: int,
    mutable lit: bool,
    mutable fading: option<animation>,
    mutable timer: option<Js.Global.timeoutId>,
    // faded out and removed
    mutable gone: bool,
  }

  let current: ref<option<word>> = ref(None)

  // The orb's and the trail's glows, defined once per board. The trail's
  // region is in board units because a near-straight trail has almost no
  // height for a bounding-box region to grow from.
  let ensureGlow = board =>
    if byId("orb-glow")->Option.isNone {
      let defs = svg("defs")
      defs->Dom.Element.setInnerHTML(
        `<filter id="orb-glow" x="-3" y="-3" width="7" height="7">
          <feGaussianBlur stdDeviation="6" result="blur" />
          <feMerge>
            <feMergeNode in="blur" /><feMergeNode in="blur" /><feMergeNode in="blur" />
            <feMergeNode in="SourceGraphic" />
          </feMerge>
        </filter>
        <filter id="trail-glow" filterUnits="userSpaceOnUse" x="-100" y="-100" width="3000" height="2000">
          <feGaussianBlur stdDeviation="4" result="blur" />
          <feMerge>
            <feMergeNode in="blur" /><feMergeNode in="blur" /><feMergeNode in="SourceGraphic" />
          </feMerge>
        </filter>`,
      )
      board->Dom.Element.appendChild(~child=defs)
    }

  let buildTrail = (board, group, plan, color) => {
    let points = plan.keys->Array.filterMap(k => byId(k)->Option.map(key => center(key, board)))
    switch points->Array.get(0) {
    | Some((x0, y0)) if points->Array.length == plan.keys->Array.length =>
      let f = Float.toString
      let move = `M ${f(x0)} ${f(y0)}`
      let segs = segments(points)
      let (path, lengths, pieces) = if segs->Array.length == 0 {
        (None, [0.], [])
      } else {
        // an unstroked copy of the path, for measuring and for placing the orb
        let path = svg("path")
        path->Dom.Element.setAttribute("fill", "none")
        group->Dom.Element.appendChild(~child=path)
        // measure the length up to each key on the way to the full path
        let lengths = Array.fromInitializer(~length=points->Array.length, i => {
          let d = [move]->Array.concat(segs->Array.slice(~start=0, ~end=i))->Array.joinWith(" ")
          path->Dom.Element.setAttribute("d", d)
          path->getTotalLength
        })
        let total = lengths->Array.getUnsafe(lengths->Array.length - 1)
        let d = [move]->Array.concat(segs)->Array.joinWith(" ")
        // butt-capped pieces of the same curve, end to end, hidden until
        // reached, glowing as one
        let tail = svg("g")
        tail->Dom.Element.setAttribute("filter", "url(#trail-glow)")
        group->Dom.Element.appendChild(~child=tail)
        let n = Js.Math.max_int(1, Js.Math.ceil_int(total /. piecePx))
        let pieces = Array.fromInitializer(~length=n, i => {
          let piece = svg("path")
          piece->Dom.Element.setAttribute("d", d)
          piece->Dom.Element.setAttribute("fill", "none")
          piece->Dom.Element.setAttribute("stroke", c(color))
          piece->Dom.Element.setAttribute("stroke-opacity", "0")
          tail->Dom.Element.appendChild(~child=piece)
          let step = total /. Float.fromInt(n)
          (piece, step *. Float.fromInt(i), step *. Float.fromInt(i + 1))
        })
        (Some(path), lengths, pieces)
      }
      // the head: a coloured ball, wider than the trail, around a white-hot core
      let orb = svg("g")
      orb->Dom.Element.setAttribute("transform", `translate(${f(x0)} ${f(y0)})`)
      orb->Dom.Element.setAttribute("filter", "url(#orb-glow)")
      orb->Dom.Element.setAttribute("opacity", "0")
      let ball = svg("circle")
      ball->Dom.Element.setAttribute("r", "11")
      ball->Dom.Element.setAttribute("fill", c(color))
      orb->Dom.Element.appendChild(~child=ball)
      let core = svg("circle")
      core->Dom.Element.setAttribute("r", "5")
      core->Dom.Element.setAttribute("fill", "white")
      orb->Dom.Element.appendChild(~child=core)
      group->Dom.Element.appendChild(~child=orb)
      Some({offsets: plan.offsets, lengths, path, pieces, orb, reached: 0, drawn: 0., tail: tailPx, frame: None})
    | _ => None
    }
  }

  let build = (text, start, stop, ~from, color) =>
    byId("Traces")->Option.map(board => {
      ensureGlow(board)
      let group = svg("g")
      board->Dom.Element.appendChild(~child=group)
      let trails =
        planWord(text, from, stop)->Array.filterMap(plan => buildTrail(board, group, plan, color))
      {
        text,
        start,
        stop,
        from,
        group,
        trails,
        seen: -1,
        lit: false,
        fading: None,
        timer: None,
        gone: false,
      }
    })

  // Draw `t` up to `len` along its path, with the orb at the tip and the
  // trail fading out behind it.
  let place = (t, len) => {
    t.drawn = len
    switch t.path {
    | Some(path) =>
      let f = Float.toString
      let total = t.lengths->Array.getUnsafe(t.lengths->Array.length - 1)
      t.pieces->Array.forEach(((piece, a, b)) => {
        let shown = Js.Math.min_float(b, len) -. a
        if shown <= 0. {
          piece->Dom.Element.setAttribute("stroke-opacity", "0")
        } else {
          // a dash covering [a, a + shown] of the path
          piece->Dom.Element.setAttribute("stroke-dasharray", `0 ${f(a)} ${f(shown)} ${f(total)}`)
          // 1 at the head, 0 at the end of the tail
          let k = Js.Math.max_float(0., 1. -. (len -. (a +. shown)) /. t.tail)
          piece->Dom.Element.setAttribute("stroke-opacity", f(headOpacity *. k))
          piece->Dom.Element.setAttribute("stroke-width", f(tailWidth +. (headWidth -. tailWidth) *. k))
        }
      })
      let {x, y} = path->getPointAtLength(len)
      t.orb->Dom.Element.setAttribute("transform", `translate(${f(x)} ${f(y)})`)
    | None => ()
    }
  }

  // Run the orb to the `reached`th key (1-based; 0 hides it), easing out.
  let goto = (t, reached) =>
    if reached != t.reached {
      t.reached = reached
      t.frame->Option.forEach(Webapi.cancelAnimationFrame)
      t.frame = None
      t.orb->Dom.Element.setAttribute("opacity", reached == 0 ? "0" : "1")
      let target = reached == 0 ? 0. : t.lengths->Array.getUnsafe(reached - 1)
      let hop = reached < 2 ? 0. : target -. t.lengths->Array.getUnsafe(reached - 2)
      t.tail = Js.Math.max_float(tailPx, hop *. tailReach)
      let from = t.drawn
      let began = ref(None)
      let rec step = now => {
        let t0 = switch began.contents {
        | Some(t0) => t0
        | None =>
          began := Some(now)
          now
        }
        let k = Js.Math.min_float(1., (now -. t0) /. Float.fromInt(traceMs))
        let eased = 1. -. Js.Math.pow_float(~base=1. -. k, ~exp=3.)
        place(t, from +. (target -. from) *. eased)
        t.frame = k < 1. ? Some(Webapi.requestCancellableAnimationFrame(step)) : None
      }
      if reached == 0 {
        place(t, 0.)
      } else {
        t.frame = Some(Webapi.requestCancellableAnimationFrame(step))
      }
    }

  // A keystroke: bring the word back to full and start its fade over. If the
  // fade runs out, the word is gone.
  let refill = w => {
    w.lit = true
    w.fading->Option.forEach(cancelAnimation)
    w.timer->Option.forEach(Js.Global.clearTimeout)
    let timing = {
      "duration": fadeMs,
      "iterations": 1,
      "easing": "ease-in",
      "fill": "forwards",
    }
    w.fading = Some(w.group->Dom.Element.animate({"opacity": [1., 0.]}, timing))
    w.timer = Some(
      Js.Global.setTimeout(() => {
        w.group->Dom.Element.remove
        w.gone = true
      }, fadeMs),
    )
  }

  // Drop the word on screen, so the next sync plans it afresh.
  let forget = () => {
    current.contents->Option.forEach(w => w.group->Dom.Element.remove)
    current := None
  }

  // Bring the trails in line with the cursor: `offset` is the last character
  // typed in `text`, and `color` the colour of the word it is in.
  let sync = (~text, ~offset, ~color) => {
    switch (current.contents, wordAt(text, offset)) {
    | (Some(w), Some((start, stop))) if w.text == text && w.start == start && w.stop == stop =>
      if w.gone && offset != w.seen {
        // faded out mid-word: a fresh trail from the key just typed
        let from = Js.Math.max_int(start, offset)
        current := build(text, start, stop, ~from, color)
      }
    | (old, span) =>
      // a word left behind keeps fading from its last keystroke
      old->Option.forEach(w =>
        if !w.lit {
          w.group->Dom.Element.remove
        }
      )
      current := span->Option.flatMap(((start, stop)) => build(text, start, stop, ~from=start, color))
    }
    current.contents->Option.forEach(w => {
      w.trails->Array.forEach(t => goto(t, t.offsets->Array.filter(o => o <= offset)->Array.length))
      if offset != w.seen {
        w.seen = offset
        if offset >= w.from {
          refill(w)
        }
      }
    })
  }

  // How a pane reads: `Glow` lights up, `Flash` lights up harder (the key a
  // bigram lands on), `Shadow` sinks into the board (a skipgram, the lesser sin).
  type look = Glow | Flash | Shadow

  // A pane of frosted glass tinted with the word color, absolutely positioned
  // at `geometry` (CSS declarations) inside the #Bumps layer.
  let glass = (~look, layer, geometry, color) => {
    let col = c(color)
    let skin = switch look {
    | Glow =>
      `background: radial-gradient(circle at 30% 20%, rgba(255,255,255,0.5), transparent 60%),
        color-mix(in srgb, ${col} 35%, transparent);
      backdrop-filter: blur(6px) saturate(180%) brightness(1.3);
      -webkit-backdrop-filter: blur(6px) saturate(180%) brightness(1.3);
      border: 1.5px solid rgba(255,255,255,0.8);
      box-shadow: inset 0 1px 2px rgba(255,255,255,0.9),
        inset 0 0 12px color-mix(in srgb, ${col} 60%, white),
        0 0 14px 3px color-mix(in srgb, ${col} 70%, white),
        0 0 36px 10px color-mix(in srgb, ${col} 45%, transparent);`
    | Flash =>
      `background: radial-gradient(circle at 30% 20%, rgba(255,255,255,0.5), transparent 60%),
        color-mix(in srgb, ${col} 55%, transparent);
      backdrop-filter: blur(6px) saturate(180%) brightness(1.7);
      -webkit-backdrop-filter: blur(6px) saturate(180%) brightness(1.7);
      border: 1.5px solid rgba(255,255,255,1);
      box-shadow: inset 0 1px 2px rgba(255,255,255,0.9),
        inset 0 0 18px color-mix(in srgb, ${col} 40%, white),
        0 0 18px 6px color-mix(in srgb, ${col} 50%, white),
        0 0 48px 18px color-mix(in srgb, ${col} 80%, transparent),
        0 0 90px 30px color-mix(in srgb, ${col} 40%, transparent);`
    | Shadow =>
      `background: color-mix(in srgb, ${col} 20%, rgba(0,0,0,0.55));
      backdrop-filter: blur(4px) saturate(60%) brightness(0.45);
      -webkit-backdrop-filter: blur(4px) saturate(60%) brightness(0.45);
      border: 1px solid rgba(0,0,0,0.6);
      box-shadow: inset 0 3px 8px rgba(0,0,0,0.85),
        inset 0 -1px 1px color-mix(in srgb, ${col} 40%, transparent),
        0 8px 22px 6px rgba(0,0,0,0.7),
        0 2px 6px rgba(0,0,0,0.8);`
    }
    let pane = document->Dom.Document.createElement("div")
    pane->Dom.Element.setAttribute("style", `position: absolute; ${geometry} ${skin}`)
    layer->Dom.Element.appendChild(~child=pane)
    let _ = setTimeout(() => pane->Dom.Element.remove, bumpMs)
    pane
  }

  let bumpTiming = {
    "duration": bumpMs,
    "iterations": 1,
    "easing": "ease-out",
    "fill": "forwards",
  }

  let bumpKey = (keycode, color) => {
    switch (byId("Bumps"), byId(keycode)) {
    | (Some(layer), Some(key)) =>
      let r = key->Dom.Element.getBoundingClientRect
      let (x, y) = center(key, layer)
      let w = Dom.DomRect.width(r)
      let h = Dom.DomRect.height(r)
      let f = Float.toString
      let pane = glass(
        ~look=Flash,
        layer,
        `left: ${f(x -. w /. 2.)}px; top: ${f(y -. h /. 2.)}px; width: ${f(w)}px; height: ${f(
            h,
          )}px; border-radius: 8px;`,
        color,
      )
      let _ = pane->Dom.Element.animate(
        {"transform": ["scale(1)", "scale(1.6)"], "opacity": [1., 0.]},
        bumpTiming,
      )
    | _ => ()
    }
  }

  let bumpLine = (~look, src, dst, color) => {
    switch (byId("Bumps"), byId(src), byId(dst)) {
    | (Some(layer), Some(a), Some(b)) =>
      let (x1, y1) = center(a, layer)
      let (x2, y2) = center(b, layer)
      let len = Js.Math.sqrt((x2 -. x1) *. (x2 -. x1) +. (y2 -. y1) *. (y2 -. y1))
      let angle = Js.Math.atan2(~y=y2 -. y1, ~x=x2 -. x1, ())
      let thick = 14.
      let f = Float.toString
      let rotate = `rotate(${f(angle)}rad)`
      // anchored at the first key's center, rotated toward the second
      let pane = glass(
        ~look,
        layer,
        `left: ${f(x1)}px; top: ${f(y1 -. thick /. 2.)}px; width: ${f(len)}px; height: ${f(
            thick,
          )}px; border-radius: 9999px; transform-origin: 0 50%;`,
        color,
      )
      let _ = pane->Dom.Element.animate(
        {
          "transform": [`${rotate} scaleY(1)`, `${rotate} scaleY(2.2)`],
          "opacity": [1., 0.],
        },
        bumpTiming,
      )
    | _ => ()
    }
  }

  // Two different keys pressed by one finger. Fingers are two-letter codes from
  // `Fingering.at`, so a word's recorded alternate fingering counts.
  let sameFinger = ((a, fingerA), (b, fingerB)) => a != b && isAlpha(a) && fingerA == fingerB

  let reset = () => {
    history := []
  }

  // `finger` is the two-letter code of the finger that really presses this key,
  // which is the default for the key unless the word has a recorded alternate.
  let press = (keycode, color: color, ~finger) => {
    let key = (keycode, finger)
    if isAlpha(keycode) {
      switch history.contents->Array.get(0) {
      | Some((prev, _) as p) if sameFinger(p, key) =>
        bumpLine(~look=Glow, prev, keycode, color)
        bumpKey(keycode, color)
      | _ => ()
      }
      switch history.contents->Array.get(1) {
      | Some((src, _) as s) if sameFinger(s, key) => bumpLine(~look=Shadow, src, keycode, color)
      | _ => ()
      }
    }
    history := [key]->Array.concat(history.contents->Array.slice(~start=0, ~end=1))
  }
}

// The keys a finger covers by default in the central block, the 33 letter
// and punctuation keys between Tab and Return, by physical key. Computed once
// per finger.
let fingerKeys = {
  let zones = Dict.make()
  finger =>
    switch zones->Dict.get(finger) {
    | Some(keys) => keys
    | None =>
      let keys =
        Ansi.codes->Array.filter(code =>
          Trace.isAlpha(code) && Fingering.code(kcHF(code)) == finger
        )
      zones->Dict.set(finger, keys)
      keys
    }
}

// Each finger's colour, the same on both hands: index amber, middle emerald,
// ring sky, pinky violet. `finger` is a two-letter code such as "ri".
let fingerColor = finger =>
  switch finger->Js.String2.charAt(1) {
  | "i" => "251 191 36"
  | "m" => "52 211 153"
  | "r" => "56 189 248"
  | "p" => "167 139 250"
  | _ => "241 245 249"
  }

// A soft wash of the finger's colour over every key it covers, fading out, so
// the finger that pressed a key shows as well as the key itself. An inset
// shadow sits over the key's background, which `sim` animates on the pressed
// key, so the two don't fight.
let flashFinger = finger => {
  let tint = alpha => `rgb(${fingerColor(finger)} / ${alpha})`
  let timing = {
    "duration": 600,
    "iterations": 1,
    "easing": "ease-out",
  }
  fingerKeys(finger)->Array.forEach(code =>
    switch document->Webapi.Dom.Document.getElementById(code) {
    | Some(key) =>
      let _ = key->Webapi.Dom.Element.animate(
        {"boxShadow": [`inset 0 0 0 60px ${tint("0.35")}`, `inset 0 0 0 60px ${tint("0")}`]},
        timing,
      )
    | None => ()
    }
  )
}

let cmpH = (a, b) => {
  switch (a, b) {
  | (#l, #r) => -1
  | (#r, #l) => 1
  | (_, _) => 0
  }
}

let cmpHF = ((h1, f1), (h2, f2)) => {
  let res = cmpH(h1, h2)
  if res != 0 {
    res
  } else {
    let order = switch h1 {
    | #l => [#p, #r, #m, #i, #t]
    | #r => [#t, #i, #m, #r, #p]
    }
    let f1 = order->Array.indexOf(f1)
    let f2 = order->Array.indexOf(f2)
    f1 < f2 ? -1 : f1 > f2 ? 1 : 0
  }
}
let color = (hfCurr, hfNext) => {
  switch hfCurr {
  | (_, #t) => #white
  | _ =>
    let res = cmpHF(hfCurr, hfNext)
    if res < 0 {
      #blue
    } else if res > 0 {
      #red
    } else {
      #yellow1
    }
  }
}

let nextColor = {
  let rec wheel = list{#red, #yellow1, #green, #blue, #indigo, ...wheel}
  let state = ref(wheel)
  () => {
    switch state.contents {
    | list{} => assert false
    | list{hd, ...rest} =>
      state.contents = rest
      hd
    }
  }
}

let runText = (text, setOffset) => {
  let data = {
    let text = text->String.split("")->Array.map(x => Some(x))
    let a = [None]->Array.concat(text)
    let b = a->Array.sliceToEnd(~start=1)->Array.concat([None])
    let c = b->Array.sliceToEnd(~start=1)->Array.concat([None])
    let a = a->List.fromArray
    let b = b->List.fromArray
    let c = c->List.fromArray
    let ab = List.zip(a, b)
    let color = ref(nextColor())
    List.zipBy(ab, c, ((a, b), c) => (a, b, c))
    ->List.toArray
    // // ->Array.flatMap(((prev, curr, next)) => {
    // //   switch (curr, next) {
    // //   | (Some(curr), Some(next)) =>
    // //     let hfCurr = Ansi.graphite2Keycode(curr)->kcHF
    // //     let hfNext = Ansi.graphite2Keycode(next)->kcHF
    // //     switch (hfCurr, hfNext) {
    // //     | ((_, #t), _)
    // //     | (_, (_, #t)) => [(prev, Some(curr), Some(next))]
    // //     | ((#l, _), (#r, _))
    // //     | ((#r, _), (#l, _)) => [(prev, Some(curr), None), (Some(curr), None, Some(next))]
    // //     | (_, _) => [(prev, Some(curr), Some(next))]
    // //     }
    // //   | (_, _) => [(prev, curr, next)]
    // //   }
    // // })
    // ->Array.map(((prev, curr, next)) => {
    //   switch (prev, curr, next) {
    //   | (_, None, _) => None
    //   | (None, Some(curr), None) =>
    //     let kcCurr = Ansi.graphite2Keycode(curr)
    //     Some(kcCurr, #white)
    //   | (Some(prev), Some(curr), Some(" ")) =>
    //     let kcPrev = Ansi.graphite2Keycode(prev)
    //     let kcCurr = Ansi.graphite2Keycode(curr)
    //     let c = color(kcPrev->kcHF, kcCurr->kcHF)
    //     Some(kcCurr, c)
    //   | (_, Some(curr), Some(next)) =>
    //     let kcCurr = Ansi.graphite2Keycode(curr)
    //     let kcNext = Ansi.graphite2Keycode(next)
    //     let c = color(kcCurr->kcHF, kcNext->kcHF)
    //     Some(kcCurr, c)
    //   | (Some(prev), Some(curr), None) =>
    //     let kcPrev = Ansi.graphite2Keycode(prev)
    //     let kcCurr = Ansi.graphite2Keycode(curr)
    //     let c = color(kcPrev->kcHF, kcCurr->kcHF)
    //     Some(kcCurr, c)
    //   }
    // })
    ->Array.map(((prev, curr, _next)) => {
      let _ = switch prev {
      | Some(" ") => color.contents = nextColor()
      | _ => ()
      }
      curr->Option.map(curr => {
        let kcCurr = Ansi.graphite2Keycode(curr)
        let c = color.contents
        (kcCurr, c)
      })
    })
    ->Array.keepSome
  }
  data->Array.reduceWithIndex(Promise.resolve(), (p, (kcCurr, color), i) => {
    p->Promise.then(() => {
      setOffset(_ => i)
      runKeycode(kcCurr, color)
    })
  })
}

module Keyboard = {
  type t = [
    | #Ansi(Ansi.t)
  ]

  @get external body: Dom.document => Dom.element = "body"

  // How a key press becomes a character. Translate: the OS is on QWERTY, and
  // each physical key is read through the board's layout. Passthru: the OS is
  // already on the board's layout, so the character it produced is used as is.
  type input = Translate | Passthru

  // Keys that drive the page rather than type, however keys are read. Escape
  // is left to the command palette.
  let isControl = code =>
    switch code {
    | "F1"
    | "F2"
    | "F3"
    | "F4"
    | "F5"
    | "F6"
    | "F7"
    | "F8"
    | "F9"
    | "F10"
    | "F11"
    | "F12"
    | "Tab"
    | "Backspace" => true
    | _ => false
    }

  let handleKC = (ev, ~t, ~input, ~setState, ~nextText) => {
    let code = Webapi.Dom.KeyboardEvent.code(ev)
    // A single label is a letter, typed in lower case without shift; a pair is
    // (base, shift), typed as written.
    let selectLayer = x => {
      if Webapi.Dom.KeyboardEvent.shiftKey(ev) {
        switch x->Array.get(1) {
        | None => x->Array.get(0)
        | Some(_) as res => res
        }
      } else {
        switch x {
        | [single] => Some(single->String.toLowerCase)
        | _ => x->Array.get(0)
        }
      }
    }

    // Translate: the key's physical position, read through the board's layout.
    let translated = () => {
      switch t {
      | #Ansi(ansi: Ansi.t) =>
        switch code {
        | "Escape" => None // opens the command palette
        | "F1"
        | "F2"
        | "F3"
        | "F4"
        | "F5"
        | "F6"
        | "F7"
        | "F8"
        | "F9"
        | "F10"
        | "F11"
        | "F12"
        | "Tab" =>
          Trace.reset()
          setState(((_offset, _text, color, _highlightColor)) => {
            let offset = -1
            let text = nextText()
            (offset, text, color, #white)
          })
          None
        | "Backquote" => selectLayer(ansi.tilde)
        | "Digit1" => selectLayer(ansi.one)
        | "Digit2" => selectLayer(ansi.two)
        | "Digit3" => selectLayer(ansi.three)
        | "Digit4" => selectLayer(ansi.four)
        | "Digit5" => selectLayer(ansi.five)
        | "Digit6" => selectLayer(ansi.six)
        | "Digit7" => selectLayer(ansi.seven)
        | "Digit8" => selectLayer(ansi.eight)
        | "Digit9" => selectLayer(ansi.nine)
        | "Digit0" => selectLayer(ansi.zero)
        | "Minus" => selectLayer(ansi.hyphen)
        | "Equal" => selectLayer(ansi.equal)
        | "Backspace" =>
          Trace.reset()
          setState(((offset, text, color, highlightColor)) => {
            let offset = offset > -1 ? offset - 1 : -1
            (offset, text, color, highlightColor)
          })
          None
        | "KeyQ" => selectLayer(ansi.q)
        | "KeyW" => selectLayer(ansi.w)
        | "KeyE" => selectLayer(ansi.e)
        | "KeyR" => selectLayer(ansi.r)
        | "KeyT" => selectLayer(ansi.t)
        | "KeyY" => selectLayer(ansi.y)
        | "KeyU" => selectLayer(ansi.u)
        | "KeyI" => selectLayer(ansi.i)
        | "KeyO" => selectLayer(ansi.o)
        | "KeyP" => selectLayer(ansi.p)
        | "BracketLeft" => selectLayer(ansi.open_bracket)
        | "BracketRight" => selectLayer(ansi.close_bracket)
        | "Backslash" => selectLayer(ansi.backslash)
        | "ControlRight" => None
        | "KeyA" => selectLayer(ansi.a)
        | "KeyS" => selectLayer(ansi.s)
        | "KeyD" => selectLayer(ansi.d)
        | "KeyF" => selectLayer(ansi.f)
        | "KeyG" => selectLayer(ansi.g)
        | "KeyH" => selectLayer(ansi.h)
        | "KeyJ" => selectLayer(ansi.j)
        | "KeyK" => selectLayer(ansi.k)
        | "KeyL" => selectLayer(ansi.l)
        | "Semicolon" => selectLayer(ansi.semicolon)
        | "Quote" => selectLayer(ansi.quote)
        | "Enter" => None
        | "ShiftLeft" => None
        | "KeyZ" => selectLayer(ansi.z)
        | "KeyX" => selectLayer(ansi.x)
        | "KeyC" => selectLayer(ansi.c)
        | "KeyV" => selectLayer(ansi.v)
        | "KeyB" => selectLayer(ansi.b)
        | "KeyN" => selectLayer(ansi.n)
        | "KeyM" => selectLayer(ansi.m)
        | "Comma" => selectLayer(ansi.comma)
        | "Period" => selectLayer(ansi.period)
        | "Slash" => selectLayer(ansi.slash)
        | "ShiftRight" => None
        | "ControlLeft" => None
        | "AltLeft" => None
        | "OSLeft" => None
        | "Space" => Some(" ")
        | "OSRight" => None
        | "AltRight" => None
        | "ArrowUp" => None
        | "ArrowLeft" => None
        | "ArrowDown" => None
        | "ArrowRight" => None
        | _ => failwith("Invalid")
        }
      }
    }
    let (kc, char) = switch input {
    | Passthru if !isControl(code) =>
      // Passthru: take the character the OS produced, and light the key that
      // carries it on the board.
      let key = Webapi.Dom.KeyboardEvent.key(ev)
      if key->String.length == 1 {
        let onBoard = Fingering.charKey.contents->Dict.get(key->String.toLowerCase)
        (onBoard->Option.getWithDefault(code), Some(key))
      } else {
        (code, None)
      }
    | _ => (code, translated())
    }
    setState(((i, text, color, highlightColor)) => {
      switch char {
      | None => (i, text, color, highlightColor)
      | Some(char) =>
        let offset = i + 1
        let curr = text->Js.String2.charAt(offset)
        if char == curr {
          let color = if kc == "Space" {
            nextColor()
          } else {
            color
          }
          let finger = Fingering.at(text, offset, kc)
          sim(kc, color)
          flashFinger(finger)
          Trace.press(kc, color, ~finger)
          (i + 1, text, color, #white)
        } else {
          (i + 1, text, color, #red)
        }
      }
    })
  }

  let onKeyDown = (ev: Webapi.Dom.KeyboardEvent.t, ~t: t, ~input, ~setState, ~nextText) => {
    module K = Webapi.Dom.KeyboardEvent
    let target = K.target(ev)
    let body = document->body
    if K.metaKey(ev) || K.ctrlKey(ev) || K.altKey(ev) {
      () // a shortcut, not typing
    } else if Obj.magic(target) === body {
      ev->Webapi.Dom.KeyboardEvent.preventDefault
      ev->handleKC(~t, ~input, ~setState, ~nextText)
    } else {
      Js.log(ev)
    }
  }

  let useListener = (t, ~input, ~setState, ~nextText) => {
    module Dom = Webapi.Dom
    let onKeyDown = React.useMemo4(() => {
      onKeyDown(~t, ~input, ~setState, ~nextText)
    }, (t, input, setState, nextText))
    React.useEffect1(() => {
      window->Dom.Window.addKeyDownEventListener(onKeyDown)
      Some(() => window->Dom.Window.removeKeyDownEventListener(onKeyDown))
    }, [onKeyDown])
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
    "ArrowUp",
    "ArrowLeft",
    "ArrowDown",
    "ArrowRight",
  ]

  // The bare ANSI key grid, with the overlay svg the Trace module draws into.
  // Anything extra goes in as children, positioned against the same wrapper.
  module Grid = {
    @react.component
    let make = (~t: t, ~children: React.element=React.null) =>
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
        <div className="relative p-[6px] bg-slate-700 rounded-2xl shadow-lg shadow-black">
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
              <Key.Power />
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
              <Key.Nav />
            </div>
          </div>
          <svg id="Traces" className="absolute inset-0 w-full h-full pointer-events-none" />
          <div id="Bumps" className="absolute inset-0 pointer-events-none" />
          children
        </div>
      }
  }

  @react.component
  let make = (~t: t, ~input, ~setState, ~nextText) => {
    useListener(t, ~input, ~setState, ~nextText)
    <Grid t />
  }
}

type fingeringFile = {
  version: int,
  entries: array<Fingering.entry>,
}

@scope("JSON") @val external parseFingerings: string => fingeringFile = "parse"

type props = {
  corpus: corpus,
  fingerings: array<Fingering.entry>,
}

let getStaticProps: Next.GetStaticProps.t<props, 'params, 'previewData> = _context => {
  let data = Node.Fs.readFileAsUtf8Sync("corpus/english.json")
  // A missing or unreadable file just means every word is fingered by default.
  // Rebuilt field by field: the parsed objects also carry the review page's
  // words, legends and notes, which would double this page's payload.
  let fingerings = try {
    parseFingerings(Node.Fs.readFileAsUtf8Sync("data/alt-fingerings.json")).entries
    ->Array.filter((e: Fingering.entry) => e.status == "alt")
    ->Array.map((e: Fingering.entry) => {
      Fingering.keys: e.keys,
      status: e.status,
      fingers: e.fingers,
    })
  } catch {
  | _ => []
  }
  let props = {corpus: parseCorpus(data), fingerings}
  Js.Promise.resolve({"props": props})
}

// A layout open in the editor. Edits change `board`; Save writes it to the
// custom layout `saved`, or to a new one when it began as a built-in.
type draft = {
  saved: option<string>,
  name: string,
  board: Ansi.t,
  // what was last saved, to tell whether anything has changed since
  savedName: string,
  savedBoard: Ansi.t,
}

@val @scope(("navigator", "clipboard"))
external writeText: string => Js.Promise.t<unit> = "writeText"

let default = (props: props) => {
  let quotes = props.corpus.quotes
  let nextText = () => {
    let quote = {
      let rand = Js.Math.random_int(0, Array.length(quotes))
      quotes->Array.getUnsafe(rand)
    }
    quote.text
  }
  let ((offset, text, color, highlightColor), setState) = React.useState(() => {
    let text = nextText()
    let color = {
      nextColor()
    }
    let offset = -1
    (offset, text, color, #white)
  })

  let (runState, setRunState) = React.useState(() => #Init)

  // Layouts saved from the editor or imported, kept in this browser.
  let (customLayouts, setCustomLayouts) = React.useState(() => [])
  let allLayouts = Ansi.layouts->Array.concat(customLayouts)
  let isCustom = id => customLayouts->Array.some(l => l.id == id)

  let (layoutId, setLayoutId) = React.useState(() => "amaranth")
  let layout =
    allLayouts
    ->Array.find(l => l.id == layoutId)
    ->Option.getWithDefault(Ansi.layouts->Array.getUnsafe(0))

  let (draft, setDraft) = React.useState(() => None)
  let (selected, setSelected) = React.useState(() => None)
  // While editing, the board shows and types with the draft.
  let ansi = switch draft {
  | Some(d) => d.board
  | None => layout.board
  }

  let (toast, setToast) = React.useState(() => None)
  let (importing, setImporting) = React.useState(() => false)
  let (importText, setImportText) = React.useState(() => "")
  let (importError, setImportError) = React.useState(() => None)
  // A layout's JSON, shown for copying by hand when the clipboard refuses it.
  let (exported, setExported) = React.useState(() => None)
  // A question waiting for a yes: (question, button label, what yes does).
  let (confirming, setConfirming) = React.useState(() => None)

  let (input, setInput) = React.useState(() => Keyboard.Translate)

  let (profileId, setProfileId) = React.useState(() => "stronglytyped")
  let profile =
    Fingering.profiles
    ->Array.find(p => p.id == profileId)
    ->Option.getWithDefault(Fingering.profiles->Array.getUnsafe(0))
  let inputName = input =>
    switch input {
    | Keyboard.Translate => "Translate"
    | Passthru => "Passthru"
    }

  // Settings are remembered per browser. Storage can be missing or refuse
  // access, in which case the page just starts on the defaults.
  let remembered = key =>
    try {
      Dom.Storage.localStorage->Dom.Storage.getItem(key, _)
    } catch {
    | _ => None
    }
  let remember = (key, value) =>
    try {
      Dom.Storage.localStorage->Dom.Storage.setItem(key, value, _)
    } catch {
    | _ => ()
    }

  // Custom layouts are stored as layout files, each with its id.
  let storeCustom = layouts =>
    remember(
      "customLayouts",
      layouts
      ->Array.map((l: Ansi.layout) =>
        Js.Json.object_(
          Js.Dict.fromArray([("id", Js.Json.string(l.id)), ("layout", Ansi.toJson(l.name, l.board))]),
        )
      )
      ->Js.Json.array
      ->Js.Json.stringify,
    )
  let loadCustom = () =>
    switch remembered("customLayouts")->Option.flatMap(text =>
      try {
        Js.Json.parseExn(text)->Js.Json.decodeArray
      } catch {
      | _ => None
      }
    ) {
    | Some(entries) =>
      entries->Array.filterMap(entry =>
        switch entry->Js.Json.decodeObject {
        | Some(o) =>
          switch (
            o->Js.Dict.get("id")->Option.flatMap(Js.Json.decodeString),
            o->Js.Dict.get("layout")->Option.map(Ansi.fromJson),
          ) {
          | (Some(id), Some(Ok((name, board)))) => Some({Ansi.id, name, board})
          | _ => None
          }
        | None => None
        }
      )
    | None => []
    }
  let updateCustom = f =>
    setCustomLayouts(layouts => {
      let next = f(layouts)
      storeCustom(next)
      next
    })
  let newId = () => "custom-" ++ Js.Date.now()->Js.Float.toFixed

  // A name no other layout has, by counting up: "My Dvorak 2", "My Dvorak 3"…
  let uniqueName = (name, ~except) => {
    let taken = n => allLayouts->Array.some(l => l.name == n && Some(l.id) != except)
    if !taken(name) {
      name
    } else {
      let i = ref(2)
      while taken(`${name} ${Int.toString(i.contents)}`) {
        i := i.contents + 1
      }
      `${name} ${Int.toString(i.contents)}`
    }
  }

  React.useEffect0(() => {
    let custom = loadCustom()
    setCustomLayouts(_ => custom)
    switch remembered("layout") {
    | Some(id)
      if Ansi.layouts->Array.some(l => l.id == id) || custom->Array.some(l => l.id == id) =>
      setLayoutId(_ => id)
    | _ => ()
    }
    switch remembered("input") {
    | Some("Passthru") => setInput(_ => Passthru)
    | _ => ()
    }
    switch remembered("fingermap") {
    | Some(id) if Fingering.profiles->Array.some(p => p.id == id) => setProfileId(_ => id)
    | _ => ()
    }
    None
  })

  // Typing and trails read characters off the layout on the board, which an
  // edit changes without changing the layout's id.
  React.useEffect1(() => {
    Fingering.setLayout(ansi)
    Trace.forget()
    None
  }, [ansi])

  React.useEffect1(() => {
    remember("layout", layoutId)
    None
  }, [layoutId])

  React.useEffect1(() => {
    switch toast {
    | Some(_) =>
      let timer = Js.Global.setTimeout(() => setToast(_ => None), 2000)
      Some(() => Js.Global.clearTimeout(timer))
    | None => None
    }
  }, [toast])

  let editDraft = f => setDraft(d => d->Option.map(f))

  let startEditing = () =>
    setDraft(_ => Some(
      if isCustom(layout.id) {
        {
          saved: Some(layout.id),
          name: layout.name,
          board: layout.board,
          savedName: layout.name,
          savedBoard: layout.board,
        }
      } else {
        {
          saved: None,
          name: "My " ++ layout.name,
          board: layout.board,
          savedName: "",
          savedBoard: layout.board,
        }
      },
    ))

  // Unsaved edits. A fresh copy of a built-in with no edits has nothing to lose.
  let changed = d => d.board != d.savedBoard || (d.saved != None && d.name != d.savedName)

  let save = () =>
    switch draft {
    | Some(d) if d.name->String.trim != "" =>
      let name = uniqueName(d.name->String.trim, ~except=d.saved)
      let id = d.saved->Option.getWithDefault(newId())
      updateCustom(layouts =>
        switch d.saved {
        | Some(_) => layouts->Array.map(l => l.id == id ? {...l, name, board: d.board} : l)
        | None => layouts->Array.concat([{Ansi.id, name, board: d.board}])
        }
      )
      setLayoutId(_ => id)
      setDraft(_ => Some({...d, saved: Some(id), name, savedName: name, savedBoard: d.board}))
      setToast(_ => Some(`Saved ${name}`))
    | _ => ()
    }

  let close = () => {
    let discard = () => {
      setDraft(_ => None)
      setSelected(_ => None)
    }
    switch draft {
    | Some(d) if changed(d) =>
      setConfirming(_ => Some((`Discard changes to ${d.name}?`, "Discard", discard)))
    | _ => discard()
    }
  }

  let delete = () =>
    switch draft {
    | Some({saved: Some(id), name}) =>
      setConfirming(_ => Some((
        `Delete ${name}?`,
        "Delete",
        () => {
          updateCustom(layouts => layouts->Array.filter(l => l.id != id))
          setLayoutId(_ => "amaranth")
          setDraft(_ => None)
          setSelected(_ => None)
          setToast(_ => Some(`Deleted ${name}`))
        },
      )))
    | _ => ()
    }

  // Copy a layout file to the clipboard, or show it when the browser refuses.
  let exportLayout = () => {
    let name = switch draft {
    | Some(d) => d.name
    | None => layout.name
    }
    let json = Ansi.stringify(name, ansi)
    let refused = () => setExported(_ => Some(json))
    let _ = try {
      writeText(json)
      ->Js.Promise.then_(() => {
        setToast(_ => Some(`Copied ${name} to the clipboard`))
        Js.Promise.resolve()
      }, _)
      ->Js.Promise.catch(_ => {
        refused()
        Js.Promise.resolve()
      }, _)
    } catch {
    | _ =>
      refused()
      Js.Promise.resolve()
    }
  }

  let importLayout = () =>
    switch Ansi.parse(importText) {
    | Ok((name, board)) =>
      let name = uniqueName(name, ~except=None)
      let id = newId()
      updateCustom(layouts => layouts->Array.concat([{Ansi.id, name, board}]))
      setLayoutId(_ => id)
      setImporting(_ => false)
      setImportText(_ => "")
      setImportError(_ => None)
      setToast(_ => Some(`Imported ${name}`))
    | Error(message) => setImportError(_ => Some(message))
    }

  let (drag, onPointerDown) = Editor.useDrag(
    ~onSwap=(a, b) => editDraft(d => {...d, board: Ansi.swap(d.board, a, b)}),
    ~onSelect=code => setSelected(_ => Some(code)),
  )
  let editor = draft->Option.map(_ => {
    Editor.editable: code => Ansi.codes->Array.includes(code),
    selected,
    source: drag->Option.flatMap((d: Editor.drag) => d.moved ? Some(d.code) : None),
    target: drag->Option.flatMap((d: Editor.drag) =>
      d.target->Option.flatMap(t => t != d.code ? Some(t) : None)
    ),
    onPointerDown,
  })

  React.useEffect1(() => {
    remember("input", inputName(input))
    None
  }, [input])

  // Trails and bumps ask the profile which finger presses each key.
  React.useEffect1(() => {
    Fingering.setProfile(profileId)
    Trace.forget()
    remember("fingermap", profileId)
    None
  }, [profileId])

  let exportCommand = {
    Palette.label: "Export layout",
    hint: "copy to the clipboard",
    checked: false,
    action: Run(exportLayout),
  }
  // While editing, the layout stays put until the editor is closed.
  let layoutCommands = switch draft {
  | Some(_) => [exportCommand]
  | None => [
      {
        Palette.label: "Choose layout…",
        hint: layout.name,
        checked: false,
        action: Open({
          title: "Layout",
          placeholder: "Choose a layout…",
          items: allLayouts->Array.map(l => {
            Palette.label: l.name,
            hint: isCustom(l.id) ? "custom" : "",
            checked: l.id == layoutId,
            action: Run(() => setLayoutId(_ => l.id)),
          }),
        }),
      },
      {
        label: "Edit layout",
        hint: layout.name,
        checked: false,
        action: Run(startEditing),
      },
      exportCommand,
      {
        label: "Import layout…",
        hint: "paste a layout file",
        checked: false,
        action: Run(() => setImporting(_ => true)),
      },
    ]
  }

  let commands: Palette.page = {
    title: "Commands",
    placeholder: "Type a command…",
    items: layoutCommands->Array.concat([
      {
        label: "Choose input…",
        hint: inputName(input),
        checked: false,
        action: Open({
          title: "Input",
          placeholder: "Choose how key presses are read…",
          items: [
            {
              Palette.label: "Translate",
              hint: `OS on QWERTY, keys mapped to ${layout.name}`,
              checked: input == Translate,
              action: Run(() => setInput(_ => Translate)),
            },
            {
              label: "Passthru",
              hint: `OS already on ${layout.name}, keys used as typed`,
              checked: input == Passthru,
              action: Run(() => setInput(_ => Passthru)),
            },
          ],
        }),
      },
      {
        label: "Choose fingermap…",
        hint: profile.name,
        checked: false,
        action: Open({
          title: "Fingermap",
          placeholder: "Choose a fingermap…",
          items: Fingering.profiles->Array.map(p => {
            Palette.label: p.name,
            hint: p.alternates
              ? "default fingers plus recorded alternates"
              : "default finger for every key",
            checked: p.id == profileId,
            action: Run(() => setProfileId(_ => p.id)),
          }),
        }),
      },
    ]),
  }

  // The table is read inside keypress handlers, so it only has to be in place
  // before the first one; an effect keeps it off the server render path.
  React.useEffect1(() => {
    Fingering.install(props.fingerings)
    None
  }, [props.fingerings])

  React.useEffect5(() => {
    if Trace.showTrails {
      Trace.sync(~text, ~offset, ~color)
    }
    None
  }, (text, offset, color, ansi, profileId))

  // React.useEffect1(() => {
  //   switch runState {
  //   | #Running =>
  //     let _ = runText(text, setOffset)->Promise.thenResolve(() => setRunState(_ => #Ready))
  //   | #Init | #Ready => ()
  //   }

  //   None
  // }, [runState])

  // Buttons that keep focus where it was, so typing carries on afterwards.
  let button = (~label, ~onClick, ~disabled=false, ~danger=false, ()) =>
    <button
      className={"rounded px-3 py-1 disabled:opacity-40 " ++ (
        danger
          ? "text-red-300 hover:bg-red-950/60"
          : "bg-slate-800 text-slate-100 hover:bg-slate-700"
      )}
      disabled
      onMouseDown={e => ReactEvent.Mouse.preventDefault(e)}
      onClick={_ => onClick()}>
      {React.string(label)}
    </button>

  <div className="flex bg-b flex-col gap-8 w-[942px]">
    {switch draft {
    | Some(d) =>
      <div className="flex items-center gap-3 font-mono text-sm text-slate-700">
        <span> {React.string("Editing")} </span>
        <input
          className="w-56 rounded bg-slate-800 px-2 py-1 text-slate-100 outline-none ring-1 ring-white/10 focus:ring-sky-400"
          value=d.name
          placeholder="Layout name"
          onChange={e => {
            let name = ReactEvent.Form.target(e)["value"]
            editDraft(d => {...d, name})
          }}
          onKeyDown={e =>
            switch ReactEvent.Keyboard.key(e) {
            | "Enter" => save()
            | "Escape" => ReactEvent.Keyboard.target(e)["blur"](.)
            | _ => ()
            }}
        />
        <span className="whitespace-nowrap text-xs text-slate-600">
          {React.string("drag to swap · click to edit")}
        </span>
        <div className="ml-auto flex gap-2">
          {button(~label="Save", ~onClick=save, ~disabled=d.name->String.trim == "", ())}
          {button(~label="Export", ~onClick=exportLayout, ())}
          {d.saved != None ? button(~label="Delete", ~onClick=delete, ~danger=true, ()) : React.null}
          {button(~label="Close", ~onClick=close, ())}
        </div>
      </div>
    | None =>
      <div className="flex justify-end gap-3 font-mono text-sm text-slate-700">
        <span>
          {React.string(
            [layout.name, inputName(input)->String.toLowerCase, profile.name]->Array.joinWith(
              " · ",
            ),
          )}
        </span>
        <span className="text-slate-600"> {React.string("esc for commands")} </span>
      </div>
    }}
    <button
      className="bg-slate-800 p-6 rounded-xl shadow-2xl text-3xl font-mono text-white"
      onClick={_ => setRunState(_ => #Running)}
      disabled={runState == #Running}>
      {React.array(
        text
        ->String.split("")
        ->Array.mapWithIndex((char, i) => {
          let className = {
            if i <= offset {
              switch highlightColor {
              | #white => "text-slate-100"
              | #red => "text-red-500"
              }
            } else {
              "text-slate-600"
            }
          }
          <span className> {React.string(char)} </span>
        }),
      )}
    </button>
    <Editor.Provider value=editor>
      <Keyboard t={#Ansi(ansi)} input setState nextText />
    </Editor.Provider>
    {switch drag {
    | Some(d) if d.moved =>
      <Editor.Ghost at=d.at label={Ansi.labelsOf(ansi, d.code)->Option.getWithDefault([])} />
    | _ => React.null
    }}
    {switch (draft, selected) {
    | (Some(_), Some(code)) =>
      let (base, shift) =
        Ansi.labelsOf(ansi, code)->Option.map(Ansi.layers)->Option.getWithDefault(("", ""))
      let also = c =>
        Ansi.keycodes(ansi)
        ->Array.find(((other, labels)) => {
          let (b, s) = Ansi.layers(labels)
          other != code && (b == c || s == c)
        })
        ->Option.map(fst)
      <Editor.Panel
        code
        base
        shift
        also
        onChange={(base, shift) =>
          editDraft(d => {...d, board: Ansi.withKey(d.board, code, Ansi.labelsFor(base, shift))})}
        onClose={() => setSelected(_ => None)}
      />
    | _ => React.null
    }}
    {importing
      ? <Editor.Dialog title="Import layout" onClose={() => setImporting(_ => false)}>
          <textarea
            autoFocus=true
            className="h-48 resize-none rounded bg-slate-900 p-3 text-xs text-slate-100 outline-none ring-1 ring-white/10 focus:ring-sky-400"
            placeholder="Paste a layout"
            value=importText
            onChange={e => {
              let text = ReactEvent.Form.target(e)["value"]
              setImportText(_ => text)
              setImportError(_ => None)
            }}
          />
          {switch importError {
          | Some(message) => <div className="text-red-300"> {React.string(message)} </div>
          | None => React.null
          }}
          <div className="flex justify-end gap-2">
            {button(~label="Cancel", ~onClick=() => setImporting(_ => false), ())}
            {button(~label="Import", ~onClick=importLayout, ~disabled=importText->String.trim == "", ())}
          </div>
        </Editor.Dialog>
      : React.null}
    {switch exported {
    | Some(json) =>
      <Editor.Dialog
        title="The clipboard isn't available here. Copy the layout below."
        onClose={() => setExported(_ => None)}>
        <textarea
          autoFocus=true
          readOnly=true
          className="h-48 resize-none rounded bg-slate-900 p-3 text-xs text-slate-100 outline-none ring-1 ring-white/10"
          value=json
          onFocus={e => ReactEvent.Focus.target(e)["select"](.)}
        />
        <div className="flex justify-end">
          {button(~label="Done", ~onClick=() => setExported(_ => None), ())}
        </div>
      </Editor.Dialog>
    | None => React.null
    }}
    {switch confirming {
    | Some((question, label, yes)) =>
      let dismiss = () => setConfirming(_ => None)
      <Editor.Dialog title=question onClose=dismiss>
        <div className="flex justify-end gap-2">
          {button(~label="Cancel", ~onClick=dismiss, ())}
          <button
            autoFocus=true
            className="rounded bg-red-900/70 px-3 py-1 text-red-100 outline-none hover:bg-red-800 focus:ring-1 focus:ring-red-300"
            onClick={_ => {
              dismiss()
              yes()
            }}>
            {React.string(label)}
          </button>
        </div>
      </Editor.Dialog>
    | None => React.null
    }}
    {switch toast {
    | Some(message) => <Editor.Toast message />
    | None => React.null
    }}
    <Palette root=commands />
  </div>
}
