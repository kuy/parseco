open Core.Std

let token str target pos =
  let len = String.length str in
  if String.slice target pos (pos + len) = str
    then (Some str, pos + len)
    else (None, pos)

let () =
  let hoge = token "hoge" in
  assert ((hoge "hoge.foo.bar" 0) = (Some "hoge", 4));
  assert ((hoge "foo.hoge.bar" 4) = (Some "hoge", 8));
  assert ((hoge "foo.bar" 0) = (None, 0));
