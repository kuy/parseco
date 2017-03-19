open! Core.Std
open Re2.Std

let clamp i (s, e) =
  if i < s then s else if e < i then e else i

let sub str start = function
  | 0 -> ""
  | len ->
    let range = (0, String.length str) in
    let s = clamp start range in
    let e = clamp (start + len) range in
    String.slice str s e

let token str target pos =
  let len = String.length str in
  if sub target pos len = str
  then (Some str, pos + len)
  else (None, pos)

let many parser target pos =
  let rec aux acc pos =
    match parser target pos with
    | (Some r, pos') -> aux (r :: acc) pos'
    | (None, pos') -> (Some acc, pos')
  in aux [] pos

let choice parsers target pos =
  let rec loop i =
    match List.nth parsers i with
    | Some parser ->
      (
        match parser target pos with
        | (None, _) -> loop (i + 1)
        | (Some r, pos') -> (Some r, pos')
      )
    | None -> (None, pos)
  in loop 0

let seq parsers target pos =
  let rec aux acc i pos' =
    match List.nth parsers i with
    | Some parser ->
      (
        match parser target pos' with
        | (Some r, pos'') -> aux (r :: acc) (i + 1) pos''
        | (None, _) -> (None, pos)
      )
    | None -> (Some (List.rev acc), pos')
  in aux [] 0 pos

let option parser target pos =
  match parser target pos with
  | (Some r, pos') -> (Some r, pos')
  | (None, pos') -> (Some "", pos')

let find_first' pat str =
  let pat' = Re2.of_string pat in
  match Re2.find_first pat' str with
  | Ok r ->
    let len = String.length r in
    if r = sub str 0 len
    then Some r
    else None
  | Error _ -> None

let regex pattern target pos =
  let target' = sub target pos (String.length target) in
  match find_first' pattern target' with
  | Some r -> (Some r, pos + (String.length r))
  | None -> (None, pos)

let character chars target pos =
  match sub target pos 1 with
  | "" -> (None, pos)
  | c ->
    if String.contains chars (String.nget c 0)
    then (Some c, pos + 1)
    else (None, pos)

let () =
  assert ((clamp 0 (0, 2)) = 0);
  assert ((clamp 1 (0, 2)) = 1);
  assert ((clamp 2 (0, 2)) = 2);
  assert ((clamp (-1) (0, 2)) = 0);
  assert ((clamp 3 (0, 2)) = 2);
  assert ((clamp (-2) (0, 2)) = 0);
  assert ((clamp 4 (0, 2)) = 2);

  assert ((sub "01234" 0 3) = "012");
  assert ((sub "01234" 0 0) = "");
  assert ((sub "01234" 2 0) = "");
  assert ((sub "01234" 5 0) = "");
  assert ((sub "01234" 1 1) = "1");
  assert ((sub "01234" 4 1) = "4");
  assert ((sub "01234" 5 1) = "");
  assert ((sub "01234" 6 3) = "");
  assert ((sub "01234" 0 4) = "0123");
  assert ((sub "01234" 0 5) = "01234");
  assert ((sub "01234" 0 6) = "01234");

  let hoge = token "hoge" in
  assert ((hoge "hoge.foo.bar" 0) = (Some "hoge", 4));
  assert ((hoge "foo.hoge.bar" 4) = (Some "hoge", 8));
  assert ((hoge "foo.bar" 0) = (None, 0));

  let many_hoge = many hoge in
  assert ((many_hoge "hogehoge" 0) = (Some ["hoge"; "hoge"], 8));
  assert ((many_hoge "" 0) = (Some [], 0));
  assert ((many_hoge "ho" 0) = (Some [], 0));

  let foo = token "foo" in
  let bar = token "bar" in
  let foo_or_bar = choice [foo; bar] in
  assert ((foo_or_bar "foobar" 0) = (Some "foo", 3));
  assert ((foo_or_bar "foobar" 3) = (Some "bar", 6));

  let many_of_foo_or_bar = many foo_or_bar in
  assert ((many_of_foo_or_bar "foobarbarfoo" 0) = (Some ["foo"; "bar"; "bar"; "foo"], 12));
  assert ((many_of_foo_or_bar "" 0) = (Some [], 0));

  let seq_test1 = seq [hoge; foo; bar] in
  assert ((seq_test1 "hogefoobar" 0) = (Some ["hoge"; "foo"; "bar"], 10));
  assert ((seq_test1 "hogefoo" 0) = (None, 0));

  let seq_test2 = seq [] in
  assert ((seq_test2 "hoge" 0) = (Some [], 0));

  let hoge_and_foo_or_bar = seq [hoge; foo_or_bar] in
  assert ((hoge_and_foo_or_bar "hogefoo" 0) = (Some ["hoge"; "foo"], 7));
  assert ((hoge_and_foo_or_bar "hogebar" 0) = (Some ["hoge"; "bar"], 7));
  assert ((hoge_and_foo_or_bar "hoge" 0) = (None, 0));

  let option_hoge = option hoge in
  assert ((option_hoge "hoge" 0) = (Some "hoge", 4));
  assert ((option_hoge "fuga" 0) = (Some "", 0));

  let mac = token "Mac" in
  let variation = seq [option mac; token "Vim"] in
  assert ((variation "MacVim" 0) = (Some ["Mac"; "Vim"], 6));
  assert ((variation "Vim" 0) = (Some [""; "Vim"], 3));
  assert ((variation "MacEmacs" 0) = (None, 0));
  assert ((variation "Emacs" 0) = (None, 0));

  assert ((find_first' "[1-9][0-9]*" "123") = (Some "123"));
  assert ((find_first' "[1-9][0-9]*" "0123") = None);
  assert ((find_first' "[1-9][0-9]*" "abc") = None);

  let num = regex "[1-9][0-9]*" in
  assert ((num "2017" 0) = (Some "2017", 4));
  assert ((num "9am" 0) = (Some "9", 1));
  assert ((num "02" 0) = (None, 0));

  let ops = character "+-*/" in
  assert ((ops "+" 0) = (Some "+", 1));
  assert ((ops "-" 0) = (Some "-", 1));
  assert ((ops "%" 0) = (None, 0));
  assert ((ops "" 0) = (None, 0));
