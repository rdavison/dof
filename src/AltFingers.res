// A review queue for words that contain a same-finger bigram under the
// amaranth layout, so alternate fingerings can be recorded by hand.
//
// Alt fingerings are stored against physical keys -- KeyboardEvent codes, i.e.
// the legends of an Apple Magic Keyboard US ANSI board -- and never against the
// letters the active layout happens to map onto them, so the data outlives a
// layout change.

module Grid = Keeb.Keyboard.Grid
module WDom = Webapi.Dom

type hand = [#l | #r]
type finger = [#p | #r | #m | #i | #t]
type hf = (hand, finger)

let hf: string => hf = keycode => Keeb.kcHF(keycode)

// Fingers are recorded as two-letter codes, and spelled with one digit per
// letter: the number-row key each finger rests on, so 1-4 is left pinky to
// left index and 7-0 is right index to right pinky. That leaves 5 and 6, the
// two keys nothing reaches from home, for the thumbs.
let fingerCode = (hf: hf) =>
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

let codeOfDigit = d =>
  switch d {
  | "1" => Some("lp")
  | "2" => Some("lr")
  | "3" => Some("lm")
  | "4" => Some("li")
  | "7" => Some("ri")
  | "8" => Some("rm")
  | "9" => Some("rr")
  | "0" => Some("rp")
  | "5" => Some("lt")
  | "6" => Some("rt")
  | _ => None
  }

let digitOfCode = c =>
  switch c {
  | "lp" => "1"
  | "lr" => "2"
  | "lm" => "3"
  | "li" => "4"
  | "ri" => "7"
  | "rm" => "8"
  | "rr" => "9"
  | "rp" => "0"
  | "lt" => "5"
  | "rt" => "6"
  | _ => "?"
  }

let fingerName = c =>
  switch c {
  | "lp" => "left pinky"
  | "lr" => "left ring"
  | "lm" => "left middle"
  | "li" => "left index"
  | "ri" => "right index"
  | "rm" => "right middle"
  | "rr" => "right ring"
  | "rp" => "right pinky"
  | "lt" => "left thumb"
  | "rt" => "right thumb"
  | _ => c
  }

// The legend printed on the physical key, as a US ANSI Magic Keyboard has it.
// Which character sits on which physical key, read off the same record that
// draws the board -- Ansi.graphite2Keycode is Graphite proper, a different
// layout.
let layout = Keeb.Ansi.amaranth

let charKey = {
  let d = Dict.make()
  Keeb.Ansi.keycodes(layout)->Array.forEach(((keycode, labels)) =>
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

// The legend printed on the physical key, as a US ANSI Magic Keyboard has it.
let legends = {
  let d = Dict.make()
  Keeb.Ansi.keycodes(Keeb.Ansi.qwerty)->Array.forEach(((keycode, labels)) =>
    switch labels->Array.get(0) {
    | Some(label) => d->Dict.set(keycode, label->String.toLowerCase)
    | None => ()
    }
  )
  d
}

let legend = keycode =>
  switch legends->Dict.get(keycode) {
  | Some(label) => label
  | None => keycode
  }

// Every physical key the word touches, in order, or None if the layout has no
// key for one of its characters. kcHF raises on an unknown keycode, so nothing
// downstream may see a character the layout cannot type.
let wordKeys = word =>
  word
  ->String.toLowerCase
  ->String.split("")
  ->Array.reduce(Some([]), (acc, ch) =>
    switch (acc, charKey->Dict.get(ch)) {
    | (Some(keys), Some(keycode)) => Some(keys->Array.concat([keycode]))
    | _ => None
    }
  )

// Key pairs that are never worth re-fingering. Each is a one-row reach on a
// single finger, short enough that nobody reaches around it. Named by physical
// key, like everything else recorded here; the letters are what the amaranth
// layout puts on them today.
let ignored = [
  ("KeyI", "KeyK"), // l·t -- right middle, top to home
  ("KeyH", "KeyM"), // m·f -- right index, home to bottom
  ("KeyP", "Semicolon"), // w·r -- right pinky, top to home
  ("KeyL", "KeyO"), // s·c -- right ring, home to top
  ("KeyH", "KeyN"), // m·b -- right index, home to bottom
  ("KeyJ", "KeyH"), // h·m -- right index, home to home
  ("KeyF", "KeyT"), // n·j -- left index, home to top
  ("Comma", "KeyI"), // g·l -- right middle, bottom to top
  ("Comma", "KeyK"), // g·t -- right middle, bottom to home
  ("KeyW", "KeyS"), // y·i -- left ring, top to home
]

let isIgnored = (a, b) => ignored->Array.some(((x, y)) => (a == x && b == y) || (a == y && b == x))

// Adjacent positions that land on the same finger but a different key -- the
// same-finger bigrams, minus the ignored pairs above. Skipgrams are
// deliberately not counted: a key in between gives the finger time to move, so
// they rarely want a different fingering. Fingers are passed in rather than
// derived, so the same test applies to a proposed alt fingering.
let pairsUnder = (keys, fingers) => {
  let out = []
  let key = i => keys->Array.getUnsafe(i)
  let finger = i => fingers->Array.getUnsafe(i)
  for i in 0 to keys->Array.length - 2 {
    if key(i) != key(i + 1) && finger(i) == finger(i + 1) && !isIgnored(key(i), key(i + 1)) {
      out->Array.push((i, i + 1))
    }
  }
  out
}

let defaultFingers = keys => keys->Array.map(kc => fingerCode(hf(kc)))
let defaultPairs = keys => pairsUnder(keys, defaultFingers(keys))

// A finger spelling: one digit per letter, or "." to keep the default finger.
let parseSpelling = (text, defaults) => {
  let chars = text->String.trim->String.split("")->Array.filter(x => x != " ")
  if chars->Array.length != defaults->Array.length {
    Error(
      `Expected ${defaults->Array.length->Int.toString} digits, got ${chars
        ->Array.length
        ->Int.toString}`,
    )
  } else {
    let bad = ref(None)
    let fingers = chars->Array.mapWithIndex((ch, i) =>
      switch ch {
      | "." => defaults->Array.getUnsafe(i)
      | d =>
        switch codeOfDigit(d) {
        | Some(code) => code
        | None =>
          bad := Some(d)
          ""
        }
      }
    )
    switch bad.contents {
    | Some(d) => Error(`"${d}" is not a finger -- use 1-4 and 7-0, or . to keep the default`)
    | None => Ok(fingers)
    }
  }
}

// The identity of a key run, so the same awkward reach can be counted across
// every word that contains it.
let runSig = (keys, (i, j)) =>
  `${keys->Array.getUnsafe(i)}>${keys->Array.getUnsafe(j)}:${(j - i)->Int.toString}`

type entry = {
  keys: array<string>,
  qwerty: string,
  status: string, // "alt" | "none" | "skipped"
  fingers: array<string>,
  words: array<string>,
  layout: string,
  note: string,
}

type file = {
  version: int,
  entries: array<entry>,
}

// Keys are derived from the word rather than shipped with it -- 5.5k keycode
// arrays would quadruple the page payload for no information.
type item = {
  word: string,
  count: int,
}

let itemKeys = item => wordKeys(item.word)->Option.getWithDefault([])

type props = {
  queue: array<item>,
  baseline: array<entry>,
}

// Mirrors the untyped parse already used for the corpus: these records are
// plain JS objects of strings, ints and arrays, so they round-trip as is.
@scope("JSON") @val external parseFile: string => file = "parse"

let sigOf = keys => keys->Array.joinWith(" ")

let storageKey = "dof.altFingerings.v1"

let notWord = %re("/[^a-z'-]+/g")
let edges = %re("/^['-]+|['-]+$/g")

let tokens = text =>
  text
  ->String.toLowerCase
  ->String.replaceRegExp(notWord, " ")
  ->String.split(" ")
  ->Array.map(w => w->String.replaceRegExp(edges, ""))
  ->Array.filter(w => w != "")

let getStaticProps: Next.GetStaticProps.t<props, 'params, 'previewData> = _context => {
  let corpus = Keeb.parseCorpus(Node.Fs.readFileAsUtf8Sync("corpus/english.json"))

  let counts = Dict.make()
  corpus.quotes->Array.forEach(quote =>
    quote.text
    ->tokens
    ->Array.forEach(word =>
      counts->Dict.set(
        word,
        switch counts->Dict.get(word) {
        | Some(n) => n + 1
        | None => 1
        },
      )
    )
  )

  let queue =
    counts
    ->Dict.toArray
    ->Array.filterMap(((word, count)) =>
      switch wordKeys(word) {
      | Some(keys) if defaultPairs(keys)->Array.length > 0 => Some({word, count})
      | _ => None
      }
    )
    ->Array.sort((a, b) => a.count == b.count ? a.word < b.word ? -1 : 1 : b.count - a.count)

  // Entries already committed to the repo are the baseline that localStorage
  // layers on top of. A missing file just means nothing has been reviewed yet.
  let baseline = try {
    let parsed = parseFile(Node.Fs.readFileAsUtf8Sync("data/alt-fingerings.json"))
    parsed.entries
  } catch {
  | _ => []
  }

  Js.Promise.resolve({"props": {queue, baseline}})
}

@val @scope(("navigator", "clipboard"))
external writeText: string => Js.Promise.t<unit> = "writeText"

let download = (~filename, ~contents) => {
  let blob = Webapi.Blob.makeWithOptions(
    [Webapi.Blob.stringToBlobPart(contents)],
    Webapi.Blob.makeBlobPropertyBag(~_type="application/json", ()),
  )
  let url = Webapi.Url.createObjectURLFromBlob(blob)
  let a = document->WDom.Document.createElement("a")
  a->WDom.Element.setAttribute("href", url)
  a->WDom.Element.setAttribute("download", filename)
  switch WDom.HtmlElement.ofElement(a) {
  | Some(el) => el->WDom.HtmlElement.click
  | None => ()
  }
  let _ = setTimeout(() => Webapi.Url.revokeObjectURL(url), 0)
}

// The grid is styled by class, so an inline style is enough to mark a key and
// removing the attribute puts it back exactly as it was.
let paint = (keycode, bg, fg) =>
  switch document->WDom.Document.getElementById(keycode) {
  | Some(el) => el->WDom.Element.setAttribute("style", `background-color:${bg};color:${fg}`)
  | None => ()
  }

let unpaint = keycode =>
  switch document->WDom.Document.getElementById(keycode) {
  | Some(el) => el->WDom.Element.removeAttribute("style")
  | None => ()
  }

let serialize = entries => JSON.stringifyAnyWithIndent({version: 1, entries}, 2)

let button = "px-4 py-2 rounded-lg bg-slate-800 text-slate-100 hover:bg-slate-700 disabled:opacity-40"

let default = (props: props) => {
  let queue = props.queue
  let total = queue->Array.length

  let (entries, setEntries) = React.useState(() => props.baseline)
  let (hydrated, setHydrated) = React.useState(() => false)
  let (index, setIndex) = React.useState(() => 0)
  let (input, setInput) = React.useState(() => "")
  let (message, setMessage) = React.useState(() => None)
  let (history, setHistory) = React.useState(() => [])
  let (importText, setImportText) = React.useState(() => "")
  let (showImport, setShowImport) = React.useState(() => false)

  // localStorage is a bare global, so it may only be touched after mount --
  // reading it during render would break the static prerender.
  React.useEffect0(() => {
    let stored = switch Dom.Storage2.localStorage->Dom.Storage2.getItem(storageKey) {
    | None => []
    | Some(raw) =>
      try {
        parseFile(raw).entries
      } catch {
      | _ => []
      }
    }
    let storedSigs = stored->Array.map(e => sigOf(e.keys))
    setEntries(_ =>
      props.baseline
      ->Array.filter(e => !(storedSigs->Array.includes(sigOf(e.keys))))
      ->Array.concat(stored)
    )
    setHydrated(_ => true)
    None
  })

  React.useEffect2(() => {
    if hydrated {
      switch serialize(entries) {
      | Some(json) => Dom.Storage2.localStorage->Dom.Storage2.setItem(storageKey, json)
      | None => ()
      }
    }
    None
  }, (entries, hydrated))

  let reviewed = React.useMemo1(() => {
    let d = Dict.make()
    entries->Array.forEach(e => d->Dict.set(sigOf(e.keys), e))
    d
  }, [entries])

  // How many queued words share each awkward key run, computed once over the
  // whole queue so a word can be judged against the rest of the corpus.
  let runCounts = React.useMemo1(() => {
    let d = Dict.make()
    queue->Array.forEach(item => {
      let keys = itemKeys(item)
      defaultPairs(keys)->Array.forEach(
        pair => {
          let sig = runSig(keys, pair)
          d->Dict.set(
            sig,
            switch d->Dict.get(sig) {
            | Some(n) => n + 1
            | None => 1
            },
          )
        },
      )
    })
    d
  }, [queue])

  // Skip forward over anything already answered, so an import or a reload
  // lands on the first genuinely open question.
  let current = {
    let rec find = i =>
      if i >= total {
        None
      } else {
        let item = queue->Array.getUnsafe(i)
        switch reviewed->Dict.get(sigOf(itemKeys(item))) {
        | Some(_) => find(i + 1)
        | None => Some((i, item))
        }
      }
    find(index)
  }

  let keys = switch current {
  | Some((_, item)) => itemKeys(item)
  | None => []
  }
  let defaults = defaultFingers(keys)
  let pairs = pairsUnder(keys, defaults)

  React.useEffect1(() => {
    pairs->Array.forEach(((i, j)) => {
      paint(keys->Array.getUnsafe(i), Keeb.c(#red), Keeb.c(#white))
      paint(keys->Array.getUnsafe(j), Keeb.c(#red), Keeb.c(#white))
    })
    let inPair = i => pairs->Array.some(((a, b)) => a == i || b == i)
    keys->Array.forEachWithIndex((kc, i) =>
      if !inPair(i) {
        paint(kc, Keeb.c(#white), Keeb.c(#black))
      }
    )
    Some(() => keys->Array.forEach(unpaint))
  }, [keys])

  let record = (~status, ~fingers) =>
    switch current {
    | None => ()
    | Some((i, item)) =>
      let sig = sigOf(keys)
      let previous = reviewed->Dict.get(sig)
      let words = switch previous {
      | Some(e) if !(e.words->Array.includes(item.word)) => e.words->Array.concat([item.word])
      | Some(e) => e.words
      | None => [item.word]
      }
      let e = {
        keys,
        qwerty: keys->Array.map(legend)->Array.joinWith(""),
        status,
        fingers,
        words,
        layout: "amaranth",
        note: "",
      }
      setEntries(prev => prev->Array.filter(x => sigOf(x.keys) != sig)->Array.concat([e]))
      setHistory(prev => prev->Array.concat([i]))
      setIndex(_ => i + 1)
      setInput(_ => "")
    }

  let submit = () =>
    switch current {
    | None => ()
    | Some(_) =>
      if input->String.trim == "" {
        setMessage(_ => None)
        record(~status="none", ~fingers=[])
      } else {
        switch parseSpelling(input, defaults) {
        | Error(err) => setMessage(_ => Some(err))
        | Ok(fingers) =>
          let left = pairsUnder(keys, fingers)
          setMessage(_ =>
            left->Array.length > 0
              ? Some("Saved, but that fingering still repeats a finger.")
              : None
          )
          record(~status="alt", ~fingers)
        }
      }
    }

  let undo = () =>
    switch history->Array.at(-1) {
    | None => ()
    | Some(i) =>
      let sig = sigOf(itemKeys(queue->Array.getUnsafe(i)))
      setEntries(prev => prev->Array.filter(x => sigOf(x.keys) != sig))
      setHistory(prev => prev->Array.slice(~start=0, ~end=prev->Array.length - 1))
      setIndex(_ => i)
      setInput(_ => "")
      setMessage(_ => None)
    }

  let doImport = () =>
    switch try {Some(parseFile(importText).entries)} catch {
    | _ => None
    } {
    | None => setMessage(_ => Some("That is not a valid alt-fingering file."))
    | Some(incoming) =>
      let sigs = incoming->Array.map(e => sigOf(e.keys))
      setEntries(prev =>
        prev->Array.filter(e => !(sigs->Array.includes(sigOf(e.keys))))->Array.concat(incoming)
      )
      setIndex(_ => 0)
      setShowImport(_ => false)
      setImportText(_ => "")
      setMessage(_ => Some(`Imported ${incoming->Array.length->Int.toString} entries.`))
    }

  // Only answers that match a word still in the queue count towards progress;
  // entries left over from an earlier, wider queue are kept but not counted.
  let answered = React.useMemo2(() =>
    queue->Array.reduce(0, (n, item) =>
      switch reviewed->Dict.get(sigOf(itemKeys(item))) {
      | Some(_) => n + 1
      | None => n
      }
    )
  , (queue, reviewed))

  // Answers that exist only in this browser. They are invisible to anyone
  // reading data/alt-fingerings.json until they are exported, which makes the
  // committed file quietly drift behind what has actually been reviewed.
  let unexported = React.useMemo1(() => {
    let committed = Dict.make()
    props.baseline->Array.forEach(e => committed->Dict.set(sigOf(e.keys), true))
    entries->Array.reduce(0, (n, e) =>
      switch committed->Dict.get(sigOf(e.keys)) {
      | Some(_) => n
      | None => n + 1
      }
    )
  }, [entries])

  <div className="flex flex-col gap-6 w-[942px] py-10">
    <div className="flex flex-row items-baseline justify-between text-slate-900">
      <div className="text-2xl font-mono"> {React.string("alt fingerings")} </div>
      <div className="text-sm font-mono text-slate-700">
        {React.string(`${answered->Int.toString} / ${total->Int.toString} SFB words reviewed`)}
      </div>
    </div>
    {switch current {
    | None =>
      <div className="bg-slate-800 rounded-xl p-8 text-slate-100 text-xl font-mono">
        {React.string("Nothing left in the queue.")}
      </div>
    | Some((i, item)) =>
      <div className="bg-slate-800 rounded-xl p-8 flex flex-col gap-6">
        <div className="flex flex-row items-end justify-between">
          <div className="flex flex-row gap-1 font-mono">
            {item.word
            ->String.split("")
            ->Array.mapWithIndex((ch, idx) => {
              let inPair = pairs->Array.some(((a, b)) => a == idx || b == idx)
              let color = inPair ? Keeb.c(#red) : Keeb.c(#white)
              <div key={Int.toString(idx)} className="flex flex-col items-center w-8">
                <div className="text-5xl" style={ReactDOM.Style.make(~color, ())}>
                  {React.string(ch)}
                </div>
                <div className="text-xs text-slate-400 mt-2">
                  {React.string(digitOfCode(defaults->Array.getUnsafe(idx)))}
                </div>
                <div className="text-[10px] text-slate-500">
                  {React.string(legend(keys->Array.getUnsafe(idx)))}
                </div>
              </div>
            })
            ->React.array}
          </div>
          <div className="text-xs font-mono text-slate-400 text-right">
            {React.string(`#${(i + 1)->Int.toString} · ${item.count->Int.toString}x in corpus`)}
          </div>
        </div>
        <div className="flex flex-row gap-2 flex-wrap">
          {pairs
          ->Array.mapWithIndex(((a, b), idx) => {
            let shared = switch runCounts->Dict.get(runSig(keys, (a, b))) {
            | Some(n) => n - 1
            | None => 0
            }
            <div
              key={Int.toString(idx)}
              className="text-xs font-mono px-3 py-1 rounded-full bg-slate-900 text-slate-300">
              {React.string(
                `${item.word->String.charAt(a)}·${item.word->String.charAt(b)} · ${fingerName(
                    defaults->Array.getUnsafe(a),
                  )} · shared with ${shared->Int.toString} other words`,
              )}
            </div>
          })
          ->React.array}
        </div>
        <div className="flex flex-row gap-3 items-center">
          <input
            autoFocus=true
            className="bg-slate-900 text-slate-100 font-mono text-2xl tracking-[0.4em] px-4 py-3 rounded-lg w-[420px] outline-none"
            placeholder={"."->String.repeat(keys->Array.length)}
            value=input
            onChange={e => {
              let v: string = ReactEvent.Form.target(e)["value"]
              let cleaned =
                v
                ->String.split("")
                ->Array.filter(ch => ch == "." || codeOfDigit(ch)->Option.isSome)
                ->Array.slice(~start=0, ~end=keys->Array.length)
                ->Array.joinWith("")
              setMessage(_ => None)
              setInput(_ => cleaned)
            }}
            onKeyDown={e =>
              switch ReactEvent.Keyboard.key(e) {
              | "Enter" =>
                ReactEvent.Keyboard.preventDefault(e)
                submit()
              | _ => ()
              }}
          />
          <button className=button onClick={_ => submit()}>
            {React.string(input->String.trim == "" ? "No alternative (↵)" : "Save (↵)")}
          </button>
          <button className=button onClick={_ => record(~status="skipped", ~fingers=[])}>
            {React.string("Skip")}
          </button>
          <button className=button disabled={history->Array.length == 0} onClick={_ => undo()}>
            {React.string("Undo")}
          </button>
        </div>
        <div className="text-xs font-mono text-slate-400">
          {React.string(
            "1 2 3 4 = left pinky ring middle index · 7 8 9 0 = right index middle ring pinky · . = keep default · empty = no alternative exists",
          )}
        </div>
        {switch message {
        | None => React.null
        | Some(text) =>
          <div className="text-sm font-mono text-yellow-300"> {React.string(text)} </div>
        }}
      </div>
    }}
    <div className="flex justify-center">
      <Grid t={#Ansi(Keeb.Ansi.amaranth)} />
    </div>
    <div className="flex flex-row gap-3 items-center">
      <button
        className={unexported > 0
          ? "px-4 py-2 rounded-lg bg-yellow-400 text-slate-900 font-semibold hover:bg-yellow-300"
          : button}
        onClick={_ =>
          switch serialize(entries) {
          | Some(json) => download(~filename="alt-fingerings.json", ~contents=json)
          | None => ()
          }}>
        {React.string(
          unexported > 0 ? `Export JSON (${unexported->Int.toString} unsaved)` : "Export JSON",
        )}
      </button>
      <button
        className=button
        onClick={_ =>
          switch serialize(entries) {
          | Some(json) =>
            let _ = writeText(json)
            setMessage(_ => Some("Copied to clipboard."))
          | None => ()
          }}>
        {React.string("Copy JSON")}
      </button>
      <button className=button onClick={_ => setShowImport(prev => !prev)}>
        {React.string("Import")}
      </button>
      <div className="text-xs font-mono text-slate-700">
        {React.string(
          unexported > 0
            ? `${unexported->Int.toString} answer${unexported == 1
                  ? ""
                  : "s"} live only in this browser -- export to data/alt-fingerings.json`
            : "In sync with data/alt-fingerings.json",
        )}
      </div>
    </div>
    {showImport
      ? <div className="flex flex-col gap-3">
          <textarea
            className="bg-slate-800 text-slate-100 font-mono text-xs p-4 rounded-lg h-40 outline-none"
            placeholder="Paste an exported alt-fingerings.json here"
            value=importText
            onChange={e => {
              let v: string = ReactEvent.Form.target(e)["value"]
              setImportText(_ => v)
            }}
          />
          <div>
            <button className=button onClick={_ => doImport()}>
              {React.string("Load entries")}
            </button>
          </div>
        </div>
      : React.null}
  </div>
}
