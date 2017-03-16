open Core.Std

let parseHoge target position =
  if String.slice target position (position + 4) = "hoge"
    then (Some "hoge", position + 4)
    else (None, position)

let () =
  assert ((parseHoge "hoge.foo.bar" 0) = (Some "hoge", 4));
  assert ((parseHoge "foo.bar" 0) = (None, 0));
