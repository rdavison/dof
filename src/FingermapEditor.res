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

// A word's letters, each over a chip in its finger's colour. Clicking a chip
// offers the ten fingers. Save keeps the fingering as a word entry; Reset drops
// the entry so the word falls back to the default fingers and rules. Keyed by
// the word's keys in the parent, so a new word starts with no edits.
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
    let (picking, setPicking) = React.useState(() => None)
    let fingers = edits->Option.getWithDefault(resolved->Array.map(fst))
    let changed = edits->Option.isSome && fingers != resolved->Array.map(fst)
    <div className="flex flex-col gap-4">
      <div className="flex flex-wrap gap-2">
        {letters
        ->Array.mapWithIndex((ch, i) => {
          let finger = fingers->Array.getUnsafe(i)
          let edited = finger != fst(resolved->Array.getUnsafe(i))
          <div key={Int.toString(i)} className="relative flex flex-col items-center gap-1">
            <div className="text-3xl text-slate-100"> {React.string(ch)} </div>
            <button
              className="w-14 rounded px-1 py-1 text-[11px] text-slate-900"
              style={ReactDOM.Style.make(~backgroundColor=fill(finger), ())}
              onMouseDown={e => ReactEvent.Mouse.preventDefault(e)}
              onClick={_ => setPicking(p => p == Some(i) ? None : Some(i))}>
              {React.string(short(finger))}
            </button>
            <div className="text-[10px] text-slate-500">
              {React.string(edited ? "edited" : sourceLabel(snd(resolved->Array.getUnsafe(i))))}
            </div>
            {picking == Some(i)
              ? <div
                  className="absolute top-full z-20 mt-1 grid w-[360px] grid-cols-5 gap-2 rounded-lg bg-slate-900 p-2 shadow-2xl shadow-black ring-1 ring-white/10">
                  {Fingers.all
                  ->Array.map(f =>
                    <Swatch
                      key=f
                      finger=f
                      selected={f == finger}
                      onClick={() => {
                        setEdits(_ => Some(fingers->Array.mapWithIndex((g, j) => j == i ? f : g)))
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
