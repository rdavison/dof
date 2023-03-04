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
  let make = (~label: array<string>, ~shape: shape) => {
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
      className={`${shape} m-[2px] bg-slate-900 rounded-lg shadow-2xl shadow-slate-700 text-white items-center flex`}>
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

  @react.component
  let make = (~t: t) => {
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
            <Key label={["esc"]} shape={#_1_5} />
            <Key label={["F1"]} shape={#_1} />
            <Key label={["F2"]} shape={#_1} />
            <Key label={["F3"]} shape={#_1} />
            <Key label={["F4"]} shape={#_1} />
            <Key label={["F5"]} shape={#_1} />
            <Key label={["F6"]} shape={#_1} />
            <Key label={["F7"]} shape={#_1} />
            <Key label={["F8"]} shape={#_1} />
            <Key label={["F9"]} shape={#_1} />
            <Key label={["F10"]} shape={#_1} />
            <Key label={["F11"]} shape={#_1} />
            <Key label={["F12"]} shape={#_1} />
            <Key label={[""]} shape={#_1} />
          </div>
          <div className="flex flex-row">
            <Key label={tilde} shape={#_1} />
            <Key label={one} shape={#_1} />
            <Key label={two} shape={#_1} />
            <Key label={three} shape={#_1} />
            <Key label={four} shape={#_1} />
            <Key label={five} shape={#_1} />
            <Key label={six} shape={#_1} />
            <Key label={seven} shape={#_1} />
            <Key label={eight} shape={#_1} />
            <Key label={nine} shape={#_1} />
            <Key label={zero} shape={#_1} />
            <Key label={hyphen} shape={#_1} />
            <Key label={equal} shape={#_1} />
            <Key label={backspace} shape={#_1_5} />
          </div>
          <div className="flex flex-row">
            <Key label={tab} shape={#_1_5} />
            <Key label={q} shape={#_1} />
            <Key label={w} shape={#_1} />
            <Key label={e} shape={#_1} />
            <Key label={r} shape={#_1} />
            <Key label={t} shape={#_1} />
            <Key label={y} shape={#_1} />
            <Key label={u} shape={#_1} />
            <Key label={i} shape={#_1} />
            <Key label={o} shape={#_1} />
            <Key label={p} shape={#_1} />
            <Key label={open_bracket} shape={#_1} />
            <Key label={close_bracket} shape={#_1} />
            <Key label={backslash} shape={#_1} />
          </div>
          <div className="flex flex-row">
            <Key label={caps_lock} shape={#_1_75} />
            <Key label={a} shape={#_1} />
            <Key label={s} shape={#_1} />
            <Key label={d} shape={#_1} />
            <Key label={f} shape={#_1} />
            <Key label={g} shape={#_1} />
            <Key label={h} shape={#_1} />
            <Key label={j} shape={#_1} />
            <Key label={k} shape={#_1} />
            <Key label={l} shape={#_1} />
            <Key label={semicolon} shape={#_1} />
            <Key label={quote} shape={#_1} />
            <Key label={enter} shape={#_1_75} />
          </div>
          <div className="flex flex-row">
            <Key label={left_shift} shape={#_2_25} />
            <Key label={z} shape={#_1} />
            <Key label={x} shape={#_1} />
            <Key label={c} shape={#_1} />
            <Key label={v} shape={#_1} />
            <Key label={b} shape={#_1} />
            <Key label={n} shape={#_1} />
            <Key label={m} shape={#_1} />
            <Key label={comma} shape={#_1} />
            <Key label={period} shape={#_1} />
            <Key label={slash} shape={#_1} />
            <Key label={right_shift} shape={#_2_25} />
          </div>
          <div className="flex flex-row">
            <Key label={["fn"]} shape={#_1} />
            <Key label={["^"]} shape={#_1} />
            <Key label={["⌥"]} shape={#_1} />
            <Key label={["⌘"]} shape={#_1_25} />
            <Key label={[""]} shape={#_5} />
            <Key label={["⌘"]} shape={#_1_25} />
            <Key label={["⌥"]} shape={#_1} />
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
  <div className="grid grid-rows-2 gap-8">
    <Keyboard t={#Ansi(Ansi.graphite)} />
    <Keyboard t={#Ansi(Ansi.qwerty)} />
  </div>
}
