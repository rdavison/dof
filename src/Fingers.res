// Fingers, by physical key. Shared by the typing page, the layout and
// fingermap editors and /alt-fingerings, and independent of any layout: keys
// are KeyboardEvent codes throughout, and fingers two-letter codes such as
// "ri" for the right index.

type hand = [#l | #r]
type finger = [#p | #r | #m | #i | #t]

// Every key's default finger.
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

// All ten, left pinky to right pinky.
let all = ["lp", "lr", "lm", "li", "lt", "rt", "ri", "rm", "rr", "rp"]

let isFinger = f => all->Array.includes(f)

// Whether a code names a key on the board.
let isKey = keycode =>
  try {
    let _ = kcHF(keycode)
    true
  } catch {
  | _ => false
  }

// The default finger of a key, as a code.
let defaultFinger = keycode => code(kcHF(keycode))

// Fingers are also spelled with one digit per letter: the number-row key each
// finger rests on, so 1-4 is left pinky to left index and 7-0 is right index to
// right pinky. That leaves 5 and 6, the two keys nothing reaches from home, for
// the thumbs.
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

// Each finger's colour as "R G B", the same on both hands: index amber, middle
// emerald, ring sky, pinky violet, thumb rose.
let color = finger =>
  switch finger->Js.String2.charAt(1) {
  | "i" => "251 191 36"
  | "m" => "52 211 153"
  | "r" => "56 189 248"
  | "p" => "167 139 250"
  | "t" => "251 113 133"
  | _ => "241 245 249"
  }

let sigOf = keys => keys->Array.joinWith(" ")

// Key pairs that are never worth re-fingering. Each is a one-row reach on a
// single finger, short enough that nobody reaches around it. Named by physical
// key; the letters are what the amaranth layout puts on them.
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
