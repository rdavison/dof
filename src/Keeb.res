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
    q: ["V"],
    w: ["B"],
    e: ["R"],
    r: ["L"],
    t: ["F"],
    y: ["J"],
    u: ["Y"],
    i: ["O"],
    o: ["U"],
    p: ["'","<"],
    open_bracket: ["-",":"],
    close_bracket: ["=", ">"],
    backslash: ["\\", "|"],
    caps_lock: ["caps"],
    a: ["S"],
    s: ["T"],
    d: ["N"],
    f: ["H"],
    g: ["M"],
    h: ["C"],
    j: ["D"],
    k: ["E"],
    l: ["I"],
    semicolon: ["A"],
    quote: [",","\""],
    enter: ["return"],
    left_shift: ["shift"],
    z: ["Z"],
    x: ["X"],
    c: ["K"],
    v: ["W"],
    b: ["Q"],
    n: ["P"],
    m: ["G"],
    comma: ["/","+"],
    period: [".","_"],
    slash: [";","?"],
    right_shift: ["shift"],
  }

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
  | #indigo => "rgb(99 102 241"
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

  let handleKC = (ev, ~t, ~setState, ~nextText) => {
    let kc = Webapi.Dom.KeyboardEvent.code(ev)
    let selectLayer = x => {
      if Webapi.Dom.KeyboardEvent.shiftKey(ev) {
        switch x->Array.get(1) {
        | None => x->Array.get(0)
        | Some(_) as res => res
        }
      } else {
        x->Array.get(0)->Option.map(String.toLowerCase)
      }
    }

    let char = {
      switch t {
      | #Ansi(ansi: Ansi.t) =>
        switch kc {
        | "Escape"
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
          sim(kc, color)
          (i + 1, text, color, #white)
        } else {
          (i + 1, text, color, #red)
        }
      }
    })
  }

  let onKeyDown = (ev: Webapi.Dom.KeyboardEvent.t, ~t: t, ~setState, ~nextText) => {
    let target = Webapi.Dom.KeyboardEvent.target(ev)
    let body = document->body
    if Obj.magic(target) === body {
      ev->Webapi.Dom.KeyboardEvent.preventDefault
      ev->handleKC(~t, ~setState, ~nextText)
    } else {
      Js.log(ev)
    }
  }

  let useListener = (t, ~setState, ~nextText) => {
    module Dom = Webapi.Dom
    let onKeyDown = React.useMemo3(() => {
      onKeyDown(~t, ~setState, ~nextText)
    }, (t, setState, nextText))
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

  @react.component
  let make = (~t: t, ~setState, ~nextText) => {
    useListener(t, ~setState, ~nextText)
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
      </div>
    }
  }
}

type props = {corpus: corpus}

let getStaticProps: Next.GetStaticProps.t<props, 'params, 'previewData> = _context => {
  let data = Node.Fs.readFileAsUtf8Sync("corpus/english.json")
  let props = {corpus: parseCorpus(data)}
  Js.Promise.resolve({"props": props})
}

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
  let ansi = Ansi.graphite

  // React.useEffect1(() => {
  //   switch runState {
  //   | #Running =>
  //     let _ = runText(text, setOffset)->Promise.thenResolve(() => setRunState(_ => #Ready))
  //   | #Init | #Ready => ()
  //   }

  //   None
  // }, [runState])

  <div className="flex bg-b flex-col gap-8 w-[942px]">
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
    <Keyboard t={#Ansi(ansi)} setState nextText />
  </div>
}
