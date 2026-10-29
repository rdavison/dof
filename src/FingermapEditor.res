// The fingermap editor's pieces that know nothing of the page: the finger
// swatches, the word editor and the list of word entries. Keys are named by
// KeyboardEvent code and fingers by two-letter code throughout.

let short = finger =>
  (finger->Js.String2.charAt(0) == "l" ? "L " : "R ") ++
  switch finger->Js.String2.charAt(1) {
  | "p" => "pinky"
  | "r" => "ring"
  | "m" => "middle"
  | "i" => "index"
  | "t" => "thumb"
  | _ => finger
  }

let fill = finger => `rgb(${Fingers.color(finger)})`

// One finger's swatch: its colour, digit and name.
module Swatch = {
  @react.component
  let make = (~finger, ~selected=false, ~onClick) =>
    <button
      className={"flex h-12 w-16 flex-col items-center justify-center rounded-lg text-slate-900 " ++ (
        selected ? "ring-2 ring-white ring-offset-2 ring-offset-slate-800" : "opacity-80 hover:opacity-100"
      )}
      style={ReactDOM.Style.make(~backgroundColor=fill(finger), ())}
      title={Fingers.fingerName(finger)}
      onMouseDown={e => ReactEvent.Mouse.preventDefault(e)}
      onClick={_ => onClick()}>
      <span className="text-sm font-semibold"> {React.string(Fingers.digitOfCode(finger))} </span>
      <span className="text-[10px]"> {React.string(short(finger))} </span>
    </button>
}

// All ten, left pinky to right pinky, with the thumbs in the middle.
module Swatches = {
  @react.component
  let make = (~selected, ~onSelect) =>
    <div className="flex gap-2">
      {Fingers.all
      ->Array.map(f => <Swatch key=f finger=f selected={f == selected} onClick={() => onSelect(f)} />)
      ->React.array}
    </div>
}

let sourceLabel = (source: Fingermap.source) =>
  switch source {
  | Base => "default"
  | Rule => "rule"
  | Word => "word"
  }

// A word's letters, each over a chip in its finger's colour with a note under
// it. Clicking a chip offers the ten fingers. Letters in `marked` show in red,
// such as the two letters of a same-finger bigram.
module Chips = {
  @react.component
  let make = (
    ~letters: array<string>,
    ~fingers: array<string>,
    ~notes: array<string>,
    ~marked: array<bool>=[],
    ~onPick,
  ) => {
    let (picking, setPicking) = React.useState(() => None)
    <div className="flex flex-wrap gap-2">
      {letters
      ->Array.mapWithIndex((ch, i) => {
        let finger = fingers->Array.getUnsafe(i)
        let red = marked->Array.get(i)->Option.getWithDefault(false)
        <div key={Int.toString(i)} className="relative flex flex-col items-center gap-1">
          <div className={"text-3xl " ++ (red ? "text-red-400" : "text-slate-100")}>
            {React.string(ch)}
          </div>
          <button
            className="w-14 rounded px-1 py-1 text-[11px] text-slate-900"
            style={ReactDOM.Style.make(~backgroundColor=fill(finger), ())}
            onMouseDown={e => ReactEvent.Mouse.preventDefault(e)}
            onClick={_ => setPicking(p => p == Some(i) ? None : Some(i))}>
            {React.string(short(finger))}
          </button>
          <div className="text-[10px] text-slate-500">
            {React.string(notes->Array.get(i)->Option.getWithDefault(""))}
          </div>
          // Anchored to the chip's outer edge, so it stays inside the panel
          // whichever end of the word the letter is at.
          {picking == Some(i)
            ? <div
                className={"absolute top-full z-20 mt-1 grid w-[360px] grid-cols-5 gap-2 rounded-lg bg-slate-900 p-2 shadow-2xl shadow-black ring-1 ring-white/10 " ++ (
                  2 * i < letters->Array.length ? "left-0" : "right-0"
                )}>
                {Fingers.all
                ->Array.map(f =>
                  <Swatch
                    key=f
                    finger=f
                    selected={f == finger}
                    onClick={() => {
                      onPick(i, f)
                      setPicking(_ => None)
                    }}
                  />
                )
                ->React.array}
              </div>
            : React.null}
        </div>
      })
      ->React.array}
    </div>
  }
}

// A word's fingering, editable. Save keeps it as a word entry; Reset drops the
// entry so the word falls back to the default fingers and rules. Keyed by the
// word's keys in the parent, so a new word starts with no edits.
module Word = {
  @react.component
  let make = (
    ~letters: array<string>,
    ~resolved: array<(string, Fingermap.source)>,
    ~hasEntry,
    ~onSave,
    ~onReset,
  ) => {
    let (edits, setEdits) = React.useState(() => None)
    let fingers = edits->Option.getWithDefault(resolved->Array.map(fst))
    let changed = edits->Option.isSome && fingers != resolved->Array.map(fst)
    <div className="flex flex-col gap-4">
      <Chips
        letters
        fingers
        notes={resolved->Array.mapWithIndex(((f, source), i) =>
          fingers->Array.getUnsafe(i) != f ? "edited" : sourceLabel(source)
        )}
        onPick={(i, f) =>
          setEdits(_ => Some(fingers->Array.mapWithIndex((g, j) => j == i ? f : g)))}
      />
      <div className="flex gap-2">
        <button
          className="rounded bg-slate-700 px-3 py-1 text-slate-100 hover:bg-slate-600 disabled:opacity-40"
          disabled={!changed}
          onMouseDown={e => ReactEvent.Mouse.preventDefault(e)}
          onClick={_ => {
            onSave(fingers)
            setEdits(_ => None)
          }}>
          {React.string("Save word")}
        </button>
        <button
          className="rounded px-3 py-1 text-slate-300 hover:bg-slate-700 disabled:opacity-40"
          disabled={!hasEntry && !changed}
          onMouseDown={e => ReactEvent.Mouse.preventDefault(e)}
          onClick={_ => {
            setEdits(_ => None)
            if hasEntry {
              onReset()
            }
          }}>
          {React.string(hasEntry ? "Reset word" : "Undo edits")}
        </button>
      </div>
    </div>
  }
}

// One word from the review queue: a word that still repeats a finger on two
// different keys back to back. Answer it with the chips or a digit spelling
// (1-4 and 7-0, "." to keep a finger), then Save it as an alternative, say it
// has none, or skip it. Keyed by the word's keys in the parent.
module Review = {
  @react.component
  let make = (
    ~letters: array<string>,
    ~keys: array<string>,
    ~resolved: array<string>,
    ~count: int,
    ~left: int,
    ~onAnswer: (string, array<string>) => unit,
    ~onUndo: option<unit => unit>,
  ) => {
    let (edits, setEdits) = React.useState(() => None)
    let (spelling, setSpelling) = React.useState(() => "")
    let (error, setError) = React.useState(() => None)
    let fingers = edits->Option.getWithDefault(resolved)
    let changed = fingers != resolved
    let pairs = Fingers.pairsUnder(keys, fingers)
    let marked = letters->Array.mapWithIndex((_, i) =>
      pairs->Array.some(((a, b)) => a == i || b == i)
    )
    let submit = () =>
      if spelling->String.trim != "" {
        switch Fingers.parseSpelling(spelling, resolved) {
        | Ok(fingers) => onAnswer("alt", fingers)
        | Error(message) => setError(_ => Some(message))
        }
      } else if changed {
        onAnswer("alt", fingers)
      } else {
        onAnswer("none", [])
      }
    let button = (~label, ~primary=false, ~onClick, ()) =>
      <button
        className={"rounded px-3 py-1 " ++ (
          primary ? "bg-slate-700 text-slate-100 hover:bg-slate-600" : "text-slate-300 hover:bg-slate-700"
        )}
        onMouseDown={e => ReactEvent.Mouse.preventDefault(e)}
        onClick={_ => onClick()}>
        {React.string(label)}
      </button>
    <div className="flex flex-col gap-4">
      <div className="text-xs text-slate-400">
        {React.string(
          `${Int.toString(left)} word${left == 1 ? "" : "s"} left · this one comes up ${Int.toString(
              count,
            )}× in the corpus`,
        )}
      </div>
      <Chips
        letters
        fingers
        marked
        notes={fingers->Array.mapWithIndex((f, i) => f != resolved->Array.getUnsafe(i) ? "edited" : "")}
        onPick={(i, f) => {
          setEdits(_ => Some(fingers->Array.mapWithIndex((g, j) => j == i ? f : g)))
          setSpelling(_ => "")
        }}
      />
      {changed && pairs->Array.length > 0
        ? <div className="text-xs text-amber-300">
            {React.string("That fingering still repeats a finger.")}
          </div>
        : React.null}
      <div className="flex items-center gap-2">
        <input
          className="w-56 rounded bg-slate-900 px-3 py-2 tracking-[0.3em] text-slate-100 outline-none ring-1 ring-white/10 focus:ring-sky-400"
          placeholder={"."->String.repeat(letters->Array.length)}
          value=spelling
          onChange={e => {
            let v: string = ReactEvent.Form.target(e)["value"]
            setSpelling(_ =>
              v
              ->String.split("")
              ->Array.filter(ch => ch == "." || Fingers.codeOfDigit(ch)->Option.isSome)
              ->Array.slice(~start=0, ~end=letters->Array.length)
              ->Array.joinWith("")
            )
            setError(_ => None)
          }}
          onKeyDown={e =>
            switch ReactEvent.Keyboard.key(e) {
            | "Enter" =>
              ReactEvent.Keyboard.preventDefault(e)
              submit()
            | "Escape" => ReactEvent.Keyboard.target(e)["blur"](.)
            | _ => ()
            }}
        />
        {button(
          ~label=changed || spelling->String.trim != "" ? "Save (↵)" : "No alternative (↵)",
          ~primary=true,
          ~onClick=submit,
          (),
        )}
        {changed || spelling->String.trim != ""
          ? button(~label="No alternative", ~onClick=() => onAnswer("none", []), ())
          : React.null}
        {button(~label="Skip", ~onClick=() => onAnswer("skipped", []), ())}
        {switch onUndo {
        | Some(undo) => button(~label="Undo", ~onClick=undo, ())
        | None => React.null
        }}
      </div>
      {switch error {
      | Some(message) => <div className="text-xs text-amber-300"> {React.string(message)} </div>
      | None => React.null
      }}
      <div className="text-xs text-slate-500">
        {React.string(
          "1 2 3 4 = left pinky ring middle index · 7 8 9 0 = right index middle ring pinky · . = keep · empty = no alternative",
        )}
      </div>
    </div>
  }
}

// The fingermap's word entries that match `query`, spelled on the current
// layout, most recent last. Clicking one opens it; × drops it.
module Entries = {
  @react.component
  let make = (~entries: array<(string, Fingermap.word)>, ~onOpen, ~onRemove) => {
    let shown = entries->Array.slice(~start=0, ~end=60)
    <div className="flex flex-col gap-2">
      <div className="text-xs text-slate-400">
        {React.string(
          switch entries->Array.length {
          | 0 => "No word entries match."
          | n if n > 60 => `${Int.toString(n)} word entries match; showing the first 60.`
          | n => `${Int.toString(n)} word ${n == 1 ? "entry" : "entries"}`
          },
        )}
      </div>
      <div className="flex max-h-40 flex-wrap gap-2 overflow-y-auto">
        {shown
        ->Array.map(((spelled, w)) =>
          <span
            key={Fingers.sigOf(w.keys)}
            className="flex items-center gap-1 rounded-full bg-slate-900 py-1 pl-3 pr-1 text-xs text-slate-200">
            <button
              className="hover:text-white"
              onMouseDown={e => ReactEvent.Mouse.preventDefault(e)}
              onClick={_ => onOpen(spelled)}>
              {React.string(spelled)}
            </button>
            <button
              className="rounded-full px-1.5 text-slate-500 hover:bg-slate-700 hover:text-white"
              title="Remove this word entry"
              onMouseDown={e => ReactEvent.Mouse.preventDefault(e)}
              onClick={_ => onRemove(w.keys)}>
              {React.string("×")}
            </button>
          </span>
        )
        ->React.array}
      </div>
    </div>
  }
}

// A finger, or "default" to leave that side of a rule alone.
module FingerSelect = {
  @react.component
  let make = (~value: option<string>, ~onChange) =>
    <select
      className="rounded bg-slate-900 px-2 py-2 text-slate-100 outline-none ring-1 ring-white/10 focus:ring-sky-400"
      value={value->Option.getWithDefault("")}
      onChange={e => {
        let v = ReactEvent.Form.target(e)["value"]
        onChange(v == "" ? None : Some(v))
      }}>
      <option value=""> {React.string("default")} </option>
      {Fingers.all
      ->Array.map(f =>
        <option key=f value=f> {React.string(`${Fingers.digitOfCode(f)} ${short(f)}`)} </option>
      )
      ->React.array}
    </select>
}

// A rule as listed: its two keys spelled on the current layout, the finger
// for each side, and how many corpus words it changes.
type rule = {
  pair: (string, string),
  first: string,
  second: string,
  fingers: (option<string>, option<string>),
  changes: int,
}

// Key-pair rules: add one by typing its two characters and choosing a finger
// for either side; adding a rule for a pair that has one replaces it.
// `onAdd` returns an error to show, or None once the rule is in.
module Rules = {
  @react.component
  let make = (~rules: array<rule>, ~onAdd, ~onRemove) => {
    let (first, setFirst) = React.useState(() => "")
    let (second, setSecond) = React.useState(() => "")
    let (firstFinger, setFirstFinger) = React.useState(() => None)
    let (secondFinger, setSecondFinger) = React.useState(() => None)
    let (error, setError) = React.useState(() => None)
    let field = (~label, ~value, ~onChange) =>
      <label className="flex flex-col gap-1">
        <span className="text-xs text-slate-400"> {React.string(label)} </span>
        <input
          className="w-14 rounded bg-slate-900 px-2 py-1 text-center text-xl text-slate-100 outline-none ring-1 ring-white/10 focus:ring-sky-400"
          value
          onFocus={e => ReactEvent.Focus.target(e)["select"](.)}
          onChange={e => {
            onChange(Editor.lastChar(ReactEvent.Form.target(e)["value"]))
            setError(_ => None)
          }}
          onKeyDown={e =>
            if ReactEvent.Keyboard.key(e) == "Escape" {
              ReactEvent.Keyboard.target(e)["blur"](.)
            }}
        />
      </label>
    let add = () =>
      switch onAdd(first, second, firstFinger, secondFinger) {
      | Some(message) => setError(_ => Some(message))
      | None =>
        setFirst(_ => "")
        setSecond(_ => "")
        setFirstFinger(_ => None)
        setSecondFinger(_ => None)
      }
    let side = (key, finger) => finger->Option.map(f => `${key} on ${short(f)}`)
    <div className="flex flex-col gap-4">
      <div className="flex items-end gap-3">
        {field(~label="First key", ~value=first, ~onChange=c => setFirst(_ => c))}
        <FingerSelect value=firstFinger onChange={f => setFirstFinger(_ => f)} />
        <span className="pb-2 text-slate-500"> {React.string("then")} </span>
        {field(~label="Second key", ~value=second, ~onChange=c => setSecond(_ => c))}
        <FingerSelect value=secondFinger onChange={f => setSecondFinger(_ => f)} />
        <button
          className="rounded bg-slate-700 px-3 py-2 text-slate-100 hover:bg-slate-600 disabled:opacity-40"
          disabled={first == "" || second == "" || (firstFinger == None && secondFinger == None)}
          onMouseDown={e => ReactEvent.Mouse.preventDefault(e)}
          onClick={_ => add()}>
          {React.string("Add rule")}
        </button>
      </div>
      {switch error {
      | Some(message) => <div className="text-amber-300"> {React.string(message)} </div>
      | None => React.null
      }}
      {rules->Array.length == 0
        ? <div className="text-xs text-slate-400">
            {React.string(
              "No rules yet. A rule changes the fingers for two keys pressed back to back, in every word that has them.",
            )}
          </div>
        : <div className="flex flex-col gap-1">
            {rules
            ->Array.map(r => {
              let (fa, fb) = r.fingers
              <div
                key={Fingers.sigOf([fst(r.pair), snd(r.pair)])}
                className="flex items-center gap-4 rounded bg-slate-900 px-3 py-1">
                <span className="w-16 text-lg text-slate-100">
                  {React.string(`${r.first} → ${r.second}`)}
                </span>
                <span className="flex-1">
                  {React.string(
                    [side(r.first, fa), side(r.second, fb)]
                    ->Array.keepSome
                    ->Array.joinWith(" · "),
                  )}
                </span>
                <span className="text-xs text-slate-400">
                  {React.string(
                    `changes ${Int.toString(r.changes)} word${r.changes == 1 ? "" : "s"}`,
                  )}
                </span>
                <button
                  className="rounded-full px-1.5 text-slate-500 hover:bg-slate-700 hover:text-white"
                  title="Remove this rule"
                  onMouseDown={e => ReactEvent.Mouse.preventDefault(e)}
                  onClick={_ => onRemove(r.pair)}>
                  {React.string("×")}
                </button>
              </div>
            })
            ->React.array}
          </div>}
    </div>
  }
}
