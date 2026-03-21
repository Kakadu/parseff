(** Demo: arithmetic parser with operator precedence *)

[@@@ocaml.warnerror "-32"]
let ws =
  let ws_re = Re.compile (Re.Posix.re "[ \t\n\r]*") in
  fun () -> Parseff.match_regex ws_re

let eadd a b = Printf.sprintf "(%s+%s)" a b
let esub a b = Printf.sprintf "(%s-%s)" a b
let emul a b = Printf.sprintf "(%s*%s)" a b
let ediv a b = Printf.sprintf "(%s/%s)" a b
let eeq a b = Printf.sprintf "(%s=%s)" a b
let elt a b = Printf.sprintf "(%s<%s)" a b

let log fmt =
  if false then
    Printf.ksprintf (Printf.printf "%s\n") fmt
  else
    Printf.ksprintf (fun _ -> ()) fmt

let prio expr table =
  let len = Array.length table in
  let rec helper level =
    if level >= len then
      expr ()
    else
      let xs = table.(level) in
      let f =
       fun (op, f) () ->
        let _ = op () in
        let r = helper (level + 1) in
        (f, r)
      in
      let h = helper (level + 1) in
      let tl = Parseff.many (Parseff.one_of (List.map f xs)) () in
      List.fold_left (fun acc (op, r) -> op acc r) h tl
  in
  helper 0

let demo_prio () =
  let number () =
    let ans = Parseff.consume "0" in
    log "string '%s' parsed" ans;
    ans
  in
  let string s () =
    log "trying string '%s'" s;
    let ans = Parseff.consume s in
    log "string '%s' parsed" ans;
    ans
  in
  prio number
    [|
      [ (string "<", elt); (string "=", eeq) ];
      [ (string "+", eadd); (string "-", esub) ];
      [ (string "*", emul); (string "/", ediv) ];
    |]

let naive_test_cases =
  [
    ("0", "0");
    ("0+0", "(0+0)");
    ("0+0+0", "((0+0)+0)");
    ("0+0*0", "(0+(0*0)");
    ("0+0<0*0", "((0+0)<(0*0))");
  ]

let test title parser cases =
  Printf.printf "%s\n" title;
  Printf.printf "==============================\n\n";
  List.iter
    (fun (input, expected) ->
      match Parseff.parse input parser with
      | Ok result ->
          let matches = String.equal result expected in
          Format.printf "✓ %-15s -> %s %s\n%!" input result
            ( if matches then
                ""
              else
                Format.asprintf "(expected [%s])" expected
            )
      | Error { pos; error = `Expected exp } ->
          Printf.printf "✗ %-15s -> Error at %d: %s\n" input pos exp
      | Error { pos; error = `Unexpected_end_of_input } ->
          Printf.printf "✗ %-15s -> Unexpected end of input at %d\n" input pos
      | Error _ ->
          Printf.printf "✗ Unknown error\n"
    )
    cases

let () =
  let parser () =
    let lst = demo_prio () in
    let _ = ws () in
    Parseff.end_of_input ();
    lst
  in
  test "Arithmetic parser with priorities" parser naive_test_cases;
  print_newline ()
