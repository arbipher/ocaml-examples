let count = ref 0

let str_of_box_type (box_type : Oformat.box_type) = match box_type with
| Pp_hbox -> "h"
| Pp_vbox -> "v"
| Pp_hvbox  -> "hv"
| Pp_hovbox  -> "hov"
| Pp_box  -> "b"
| Pp_fits  -> "f"

let dump_boxes (fmter : Oformat.formatter) = 
  let fstk = fmter.pp_format_stack in 
  Stack.to_seq fstk |>
  Seq.map (fun (ele : Oformat.pp_format_elem ) -> Format.sprintf "(%s,%d)"  (str_of_box_type ele.box_type) ele.width) 
  |> List.of_seq
  |> List.rev
  |> String.concat ","

let dump_string_ (fmter : Oformat.formatter) =
  Format.sprintf "[ci=%d; mg=%d; ml=%d; sl=%d; lt=%d; rt=%d; cd=%d;%s]" fmter.pp_current_indent fmter.pp_margin fmter.pp_min_space_left fmter.pp_space_left 
  fmter.pp_left_total fmter.pp_right_total
  fmter.pp_curr_depth
  (dump_boxes fmter)

let dump_string (fmter_real : Format.formatter) : string =
  dump_string_ (Obj.magic fmter_real)

let dump oc (fmter_real : Format.formatter) =
  let info = dump_string_ (Obj.magic fmter_real) in
  let count_s = Format.sprintf "{%d}" !count in
  Out_channel.output_string oc count_s;
  Out_channel.output_string oc info;
  Out_channel.output_string oc "\n";
  Format.fprintf fmter_real "%s" count_s ;
  count := !count + 1;

  (* 
rm=%d;
fmter.pp_margin 

sl=%d; 
fmter.pp_min_space_left 
  *)