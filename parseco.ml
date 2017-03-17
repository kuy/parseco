open Core.Std

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
