open Core.Std

let bound n (s, e) =
  if n < s then s else if e < n then e else n

let sub str start = function
  | 0 -> ""
  | len ->
    let str_len = String.length str in
    let range = (0, str_len) in
    let i_start = bound start range in
    let i_end = bound (start + len) range in
    String.slice str i_start i_end

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

let () =
  assert ((bound 0 (0, 2)) = 0);
  assert ((bound 1 (0, 2)) = 1);
  assert ((bound 2 (0, 2)) = 2);
  assert ((bound (-1) (0, 2)) = 0);
  assert ((bound 3 (0, 2)) = 2);
  assert ((bound (-2) (0, 2)) = 0);
  assert ((bound 4 (0, 2)) = 2);

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
