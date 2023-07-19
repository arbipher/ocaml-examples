let dump_string (fmter_real : Format.formatter) : string =
  let (fmter : Oformat.formatter) = Obj.magic fmter_real in
  Format.sprintf "%d" fmter.pp_current_indent
