// A fingermap: which finger presses each key. It starts from a default finger
// per key (`base`, over the standard fingers in Fingers.kcHF). Key-pair rules
// then adjust fingers wherever two keys come up back to back, and whole-word
// entries override both for one word's exact run of keys. Keys are
// KeyboardEvent codes throughout, so a fingermap outlives a layout change.

// A rule for two keys pressed back to back: the finger for either side, or
// None to leave that side as it is.
type rule = {pair: (string, string), fingers: (option<string>, option<string>)}

// A whole word's fingering. With status "alt", `fingers` applies; "none" and
// "skipped" mark a word that was looked at and left alone. `examples` are the
// words seen with this run of keys.
type word = {keys: array<string>, fingers: array<string>, status: string, examples: array<string>}

type t = {
  id: string,
  name: string,
  // keys whose default finger differs from the standard one
  base: Dict.t<string>,
  rules: array<rule>,
  // by Fingers.sigOf of the keys
  words: Dict.t<word>,
}

let baseFinger = (map, keycode) =>
  switch map.base->Dict.get(keycode) {
  | Some(f) => f
  | None => Fingers.defaultFinger(keycode)
  }

// Where a letter's finger came from, for showing in the editor.
type source = Base | Rule | Word

// The fingers for a run of keys, and where each came from. A word entry wins
// outright. Otherwise each key starts on its base finger and rules apply left
// to right; once a rule has set a letter's finger, a later pair can't change
// it, so the result doesn't depend on the order rules were written in.
let resolveWithSources = (map, keys) => {
  let n = keys->Array.length
  switch map.words->Dict.get(Fingers.sigOf(keys)) {
  | Some({status: "alt", fingers}) if fingers->Array.length == n =>
    fingers->Array.map(f => (f, Word))
  | _ =>
    let out = keys->Array.map(k => (baseFinger(map, k), Base))
    let set = Array.make(~length=n, false)
    let place = (i, finger) =>
      switch finger {
      | Some(f) if !(set->Array.getUnsafe(i)) =>
        out->Array.setUnsafe(i, (f, Rule))
        set->Array.setUnsafe(i, true)
      | _ => ()
      }
    for i in 0 to n - 2 {
      let pair = (keys->Array.getUnsafe(i), keys->Array.getUnsafe(i + 1))
      map.rules->Array.forEach(r =>
        if r.pair == pair {
          place(i, fst(r.fingers))
          place(i + 1, snd(r.fingers))
        }
      )
    }
    out
  }
}

let resolve = (map, keys) => resolveWithSources(map, keys)->Array.map(fst)

let copy = d => d->Dict.toArray->Dict.fromArray

// `map` with `keycode`'s default finger set. Setting it back to the standard
// finger drops the override.
let withBase = (map, keycode, finger) => {
  let base = copy(map.base)
  if finger == Fingers.defaultFinger(keycode) {
    base->Dict.delete(keycode)
  } else {
    base->Dict.set(keycode, finger)
  }
  {...map, base}
}

let withWord = (map, word) => {
  let words = copy(map.words)
  words->Dict.set(Fingers.sigOf(word.keys), word)
  {...map, words}
}

let withoutWord = (map, keys) => {
  let words = copy(map.words)
  words->Dict.delete(Fingers.sigOf(keys))
  {...map, words}
}

// The built-ins. stronglytyped carries the alternates shipped in
// data/alt-fingerings.json; standard has none.
let standard = {
  id: "standard",
  name: "standard",
  base: Dict.make(),
  rules: [],
  words: Dict.make(),
}

let stronglytyped = (shipped: array<word>) => {
  ...standard,
  id: "stronglytyped",
  name: "stronglytyped",
  words: shipped->Array.map(w => (Fingers.sigOf(w.keys), w))->Dict.fromArray,
}

let isBuiltin = id => id == "standard" || id == "stronglytyped"

// A fingermap as a file, for saving, export and import.
let toJson = map => {
  let s = Js.Json.string
  let strings = xs => xs->Array.map(s)->Js.Json.array
  let finger = f =>
    switch f {
    | Some(f) => s(f)
    | None => Js.Json.null
    }
  Js.Json.object_(
    Js.Dict.fromArray([
      ("format", s("dof-fingermap")),
      ("version", Js.Json.number(1.)),
      ("name", s(map.name)),
      ("base", map.base->Dict.toArray->Array.map(((k, f)) => (k, s(f)))->Js.Dict.fromArray->Js.Json.object_),
      (
        "rules",
        map.rules
        ->Array.map(r => {
          let (a, b) = r.pair
          let (fa, fb) = r.fingers
          Js.Json.object_(
            Js.Dict.fromArray([
              ("keys", strings([a, b])),
              ("fingers", [finger(fa), finger(fb)]->Js.Json.array),
            ]),
          )
        })
        ->Js.Json.array,
      ),
      (
        "words",
        map.words
        ->Dict.valuesToArray
        ->Array.map(w =>
          Js.Json.object_(
            Js.Dict.fromArray([
              ("keys", strings(w.keys)),
              ("fingers", strings(w.fingers)),
              ("status", s(w.status)),
              ("words", strings(w.examples)),
            ]),
          )
        )
        ->Js.Json.array,
      ),
    ]),
  )
}

let stringify = map => toJson(map)->Js.Json.stringifyWithSpace(2)

exception Invalid(string)

// A fingermap file read back, as (name, base, rules, words). The error names
// what is wrong, for showing to whoever pasted it.
let fromJson = json =>
  try {
    let obj = switch json->Js.Json.decodeObject {
    | Some(o) => o
    | None => raise(Invalid("not a fingermap: expected an object"))
    }
    let get = (o, k) => o->Js.Dict.get(k)
    let list = (o, k, what) =>
      switch get(o, k) {
      | None => []
      | Some(v) =>
        switch v->Js.Json.decodeArray {
        | Some(xs) => xs
        | None => raise(Invalid(`${what} must be a list`))
        }
      }
    let strings = (o, k, what) =>
      list(o, k, what)->Array.map(v =>
        switch v->Js.Json.decodeString {
        | Some(x) => x
        | None => raise(Invalid(`${what} must be a list of strings`))
        }
      )
    let key = (k, what) => Fingers.isKey(k) ? k : raise(Invalid(`${what}: ${k} is not a key`))
    let finger = (f, what) =>
      Fingers.isFinger(f) ? f : raise(Invalid(`${what}: "${f}" is not a finger (lp … rp)`))

    if get(obj, "format")->Option.flatMap(Js.Json.decodeString) != Some("dof-fingermap") {
      raise(Invalid(`not a fingermap: "format" should be "dof-fingermap"`))
    }
    if get(obj, "version")->Option.flatMap(Js.Json.decodeNumber) != Some(1.) {
      raise(Invalid(`unsupported "version": expected 1`))
    }
    let name = switch get(obj, "name")->Option.flatMap(Js.Json.decodeString) {
    | Some(n) if n->String.trim != "" => n->String.trim
    | _ => raise(Invalid(`"name" must be a non-empty string`))
    }
    let base = switch get(obj, "base") {
    | None => Dict.make()
    | Some(v) =>
      switch v->Js.Json.decodeObject {
      | Some(o) =>
        o
        ->Js.Dict.entries
        ->Array.map(((k, f)) =>
          switch f->Js.Json.decodeString {
          | Some(f) => (key(k, "base"), finger(f, `base ${k}`))
          | None => raise(Invalid(`base ${k}: expected a finger`))
          }
        )
        ->Dict.fromArray
      | None => raise(Invalid(`"base" must be an object of key: finger`))
      }
    }
    let rules = list(obj, "rules", `"rules"`)->Array.map(v => {
      let o = switch v->Js.Json.decodeObject {
      | Some(o) => o
      | None => raise(Invalid(`each rule must be an object with "keys" and "fingers"`))
      }
      let (a, b) = switch strings(o, "keys", "rule keys") {
      | [a, b] => (key(a, "rule"), key(b, "rule"))
      | _ => raise(Invalid("a rule needs exactly two keys"))
      }
      let side = v =>
        switch v {
        | None => None
        | Some(v) =>
          switch (v->Js.Json.decodeNull, v->Js.Json.decodeString) {
          | (Some(_), _) => None
          | (_, Some(f)) => Some(finger(f, `rule ${a} ${b}`))
          | _ => raise(Invalid(`rule ${a} ${b}: each finger is a finger or null`))
          }
        }
      let fingers = list(o, "fingers", "rule fingers")
      ({pair: (a, b), fingers: (side(fingers->Array.get(0)), side(fingers->Array.get(1)))}: rule)
    })
    let words = list(obj, "words", `"words"`)->Array.map(v => {
      let o = switch v->Js.Json.decodeObject {
      | Some(o) => o
      | None => raise(Invalid(`each word must be an object with "keys" and "fingers"`))
      }
      let keys = strings(o, "keys", "word keys")->Array.map(k => key(k, "word"))
      let sig = Fingers.sigOf(keys)
      let fingers = strings(o, "fingers", "word fingers")->Array.map(f => finger(f, `word ${sig}`))
      let status = get(o, "status")->Option.flatMap(Js.Json.decodeString)->Option.getWithDefault("alt")
      if !(["alt", "none", "skipped"]->Array.includes(status)) {
        raise(Invalid(`word ${sig}: status must be alt, none or skipped`))
      }
      if status == "alt" && fingers->Array.length != keys->Array.length {
        raise(Invalid(`word ${sig}: one finger per key`))
      }
      (sig, {keys, fingers, status, examples: strings(o, "words", "word examples")})
    })
    Ok((name, base, rules, words->Dict.fromArray))
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
