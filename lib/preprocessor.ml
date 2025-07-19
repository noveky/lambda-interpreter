let preprocess_lambda ic =
  let buf = Buffer.create 1024 in
  let rec next () =
    match input_byte ic with
    | exception End_of_file -> ()
    | 0xCE -> (
      match input_byte ic with
      | 0xBB ->
        Buffer.add_char buf '\\';
        next ()
      | b ->
        Buffer.add_char buf (Char.chr 0xCE);
        Buffer.add_char buf (Char.chr b);
        next ())
    | b ->
      Buffer.add_char buf (Char.chr b);
      next ()
  in
  next ();
  Buffer.contents buf
