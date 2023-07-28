let rec repeat n prop = fun input ->
  if n<0 then failwith "repeat: negative repetition count";
  if n=0
  then true
  else prop input && repeat (n-1) prop input

exception Timeout

let prop_timeout sec p x =
  Sys.(signal sigalrm (Signal_handle (fun _ -> raise Timeout))) |> ignore;
  ignore (Unix.alarm sec);
  let res = p x in
  ignore (Unix.alarm 0); (*cancel alarm*)
  res

let fork_prop_with_timeout sec p x =
  let a = Unix.fork () in
  match a with
  | 0  ->
    let _ = Unix.alarm sec in
    if p x
    then (ignore (Unix.alarm 0); exit 0) (*cancel alarm*)
    else (ignore (Unix.alarm 0); exit 2) (*cancel alarm*)
  | _  ->
    let _childid, retcode = Unix.wait () in
    (match retcode with
     | WEXITED code -> (0=code)
     | WSIGNALED s when s = Sys.sigalrm -> raise Timeout
     | WSIGNALED _
     | WSTOPPED _  -> false)

let print_vertical ?(fig_indent=3) show cmds =
  let cmds = List.map show cmds in
  let buf = Buffer.create 64 in
  let indent () = Printf.bprintf buf "%s" (String.make fig_indent ' ') in
  let print_seq_col c = Printf.bprintf buf "%s\n" c in
  let () = List.iter (fun c -> indent (); print_seq_col c) cmds in
  Buffer.contents buf

let print_triple_vertical ?(fig_indent=10) ?(res_width=20) ?(center_prefix=true) show (seq,cmds1,cmds2) =
  let seq,cmds1,cmds2 = List.(map show seq, map show cmds1, map show cmds2) in
  let max_width ss = List.fold_left max 0 (List.map String.length ss) in
  let width = List.fold_left max 0 [max_width seq; max_width cmds1; max_width cmds2] in
  let res_width = max width res_width in
  let cmd_indent = String.make ((width-1)/2) ' ' in
  let seq_indent = String.make ((res_width + 3)/2) ' ' in
  let bar_cmd = Printf.sprintf "%-*s" res_width (cmd_indent ^ "|") in
  let center c =
    let clen = String.length c in
    if clen > width (* it's a '|'-string *)
    then c
    else Printf.sprintf "%s%s" (String.make ((width - clen)/2) ' ') c in
  let buf = Buffer.create 64 in
  let indent () = Printf.bprintf buf "%s" (String.make fig_indent ' ') in
  let print_seq_col c = Printf.bprintf buf "%s%-*s\n" seq_indent res_width c in
  let print_par_col c1 c2 = Printf.bprintf buf "%-*s  %-*s\n" res_width c1 res_width c2 in
  let print_hoz_line () =
    Printf.bprintf buf "%-*s\n" res_width (cmd_indent ^ "." ^ (String.make (res_width + 1) '-') ^ ".") in
  let rec print_par_cols cs cs' = match cs,cs' with
    | [],   []    -> ()
    | c::cs,[]    -> indent (); print_par_col (center c) ""; print_par_cols cs []
    | [],   c::cs -> indent (); print_par_col "" (center c); print_par_cols [] cs
    | l::ls,r::rs -> indent (); print_par_col (center l) (center r); print_par_cols ls rs in
  (* actual printing *)
  if center_prefix
  then
    List.iter (fun c -> indent (); print_seq_col (center c)) ([bar_cmd] @ seq @ [bar_cmd])
  else
    List.iter (fun c -> indent (); print_par_col (center c) "") (bar_cmd::seq@[bar_cmd]);
  indent (); print_hoz_line ();
  print_par_cols (bar_cmd::cmds1) (bar_cmd::cmds2);
  Buffer.contents buf

let protect (f : 'a -> 'b) (a : 'a) : ('b, exn) result =
  try Result.Ok (f a)
  with e -> Result.Error e

module Pp = struct
  open Format

  type 'a t = bool -> Format.formatter -> 'a -> unit

  type pp_thunk = Format.formatter -> unit

  let truncate_message = "... (truncated)"

  let truncate_length =
    let truncate_env = "MCTUTILS_TRUNCATE" in
    let ( let* ) = Option.bind in
    let* l = Sys.getenv_opt truncate_env in
    let* l = int_of_string_opt l in
    (* it does not make sense to truncate at less than the length of
       [truncate_message] *)
    if l > 0 then Some (max l (String.length truncate_message - 1)) else None

  let to_show f x =
    match truncate_length with
    | None ->
        let buf = Buffer.create 512 in
        let fmt = formatter_of_buffer buf in
        pp_set_margin fmt max_int;
        fprintf fmt "@[<h 0>%a@]@?" (f false) x;
        let s = Buffer.contents buf in
        Buffer.reset buf;
        s
    | Some trlen ->
        (* if we overflow, we'll have the [truncate_message] at the end of the
           buffer, filling it until [trlen + 1]: we'll use the fact that the
           buffer contains more than [trlen] to indicate that it has already
           overflown *)
        let buf = Buffer.create (trlen + 1) in
        let msglen = String.length truncate_message in
        let out str ofs len =
          let blen = Buffer.length buf in
          (* if we didn't overflow yet... *)
          if blen <= trlen then
            if blen + len > trlen then (
              let fits = trlen - blen - msglen + 1 in
              if fits > 0 then Buffer.add_substring buf str ofs fits
              else Buffer.truncate buf (trlen + 1 - msglen);
              Buffer.add_string buf truncate_message)
            else Buffer.add_substring buf str ofs len
        in
        let ppf = make_formatter out ignore in
        pp_set_margin ppf max_int;
        fprintf ppf "@[<h 0>%a@]@?" (f false) x;
        let s = Buffer.contents buf in
        Buffer.reset buf;
        s

  let of_show f par fmt x =
    fprintf fmt (if par then "@[(%s)@]" else "@[%s@]") (f x)

  let cst0 name fmt = pp_print_string fmt name

  let cst1 (pp : 'a t) name par fmt x =
    let o, c = if par then ("(", ")") else ("", "") in
    fprintf fmt "%s@[<2>%s@ %a@]%s" o name (pp true) x c

  let cst2 (pp1 : 'a t) (pp2 : 'b t) name par fmt x y =
    let o, c = if par then ("(", ")") else ("", "") in
    fprintf fmt "%s@[<2>%s (@,%a,@ %a)@]%s" o name (pp1 false) x (pp2 false) y c

  let cst3 (pp1 : 'a t) (pp2 : 'b t) (pp3 : 'c t) name par fmt x y z =
    let o, c = if par then ("(", ")") else ("", "") in
    fprintf fmt "%s@[<2>%s (@,%a,@ %a,@ %a)@]%s" o name (pp1 false) x
      (pp2 false) y (pp3 false) z c

  let pp_exn = of_show Printexc.to_string
  let pp_unit _ fmt () = pp_print_string fmt "()"
  let pp_bool _ fmt b = fprintf fmt "%B" b
  let pp_int par fmt i = fprintf fmt (if par && i < 0 then "(%d)" else "%d") i
  let pp_int32 par fmt i = fprintf fmt (if par && i < 0l then "(%ldl)" else "%ldl") i
  let pp_int64 par fmt i = fprintf fmt (if par && i < 0L then "(%LdL)" else "%LdL") i
  let pp_float par fmt f = fprintf fmt (if par && f < 0.0 then "(%F)" else "%F") f
  let pp_char _ fmt c = fprintf fmt "%C" c
  let pp_string _ fmt s = fprintf fmt "%S" s
  let pp_bytes _ fmt s = fprintf fmt "%S" (Bytes.to_string s)

  let pp_option (pp_s : 'a t) par fmt o =
    match o with
    | None -> cst0 "None" fmt
    | Some s -> cst1 pp_s "Some" par fmt s

  let pp_result (pp_o : 'o t) (pp_e : 'e t) par fmt r =
    let open Result in
    match r with
    | Ok o -> cst1 pp_o "Ok" par fmt o
    | Error e -> cst1 pp_e "Error" par fmt e

  type pp_tuple_item = pp_thunk

  let pp_tuple_item pp x fmt = pp false fmt x

  let pp_tuple _ fmt items =
    fprintf fmt "(@[";
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") (fun fmt ppf -> ppf fmt) fmt items;
    fprintf fmt "@])"

  let pp_tuple2 pp1 pp2 p fmt (x1, x2) =
    pp_tuple p fmt [ pp_tuple_item pp1 x1; pp_tuple_item pp2 x2 ]

  let pp_tuple3 pp1 pp2 pp3 p fmt (x1, x2, x3) =
    pp_tuple p fmt
      [ pp_tuple_item pp1 x1; pp_tuple_item pp2 x2; pp_tuple_item pp3 x3 ]

  let pp_tuple4 pp1 pp2 pp3 pp4 p fmt (x1, x2, x3, x4) =
    pp_tuple p fmt
      [
        pp_tuple_item pp1 x1;
        pp_tuple_item pp2 x2;
        pp_tuple_item pp3 x3;
        pp_tuple_item pp4 x4;
      ]

  let pp_tuple5 pp1 pp2 pp3 pp4 pp5 p fmt (x1, x2, x3, x4, x5) =
    pp_tuple p fmt
      [
        pp_tuple_item pp1 x1;
        pp_tuple_item pp2 x2;
        pp_tuple_item pp3 x3;
        pp_tuple_item pp4 x4;
        pp_tuple_item pp5 x5;
      ]

  let pp_tuple6 pp1 pp2 pp3 pp4 pp5 pp6 p fmt (x1, x2, x3, x4, x5, x6) =
    pp_tuple p fmt
      [
        pp_tuple_item pp1 x1;
        pp_tuple_item pp2 x2;
        pp_tuple_item pp3 x3;
        pp_tuple_item pp4 x4;
        pp_tuple_item pp5 x5;
        pp_tuple_item pp6 x6;
      ]

  let pp_tuple7 pp1 pp2 pp3 pp4 pp5 pp6 pp7 p fmt (x1, x2, x3, x4, x5, x6, x7) =
    pp_tuple p fmt
      [
        pp_tuple_item pp1 x1;
        pp_tuple_item pp2 x2;
        pp_tuple_item pp3 x3;
        pp_tuple_item pp4 x4;
        pp_tuple_item pp5 x5;
        pp_tuple_item pp6 x6;
        pp_tuple_item pp7 x7;
      ]

  let pp_tuple8 pp1 pp2 pp3 pp4 pp5 pp6 pp7 pp8 p fmt
      (x1, x2, x3, x4, x5, x6, x7, x8) =
    pp_tuple p fmt
      [
        pp_tuple_item pp1 x1;
        pp_tuple_item pp2 x2;
        pp_tuple_item pp3 x3;
        pp_tuple_item pp4 x4;
        pp_tuple_item pp5 x5;
        pp_tuple_item pp6 x6;
        pp_tuple_item pp7 x7;
        pp_tuple_item pp8 x8;
      ]

  let pp_tuple9 pp1 pp2 pp3 pp4 pp5 pp6 pp7 pp8 pp9 p fmt
      (x1, x2, x3, x4, x5, x6, x7, x8, x9) =
    pp_tuple p fmt
      [
        pp_tuple_item pp1 x1;
        pp_tuple_item pp2 x2;
        pp_tuple_item pp3 x3;
        pp_tuple_item pp4 x4;
        pp_tuple_item pp5 x5;
        pp_tuple_item pp6 x6;
        pp_tuple_item pp7 x7;
        pp_tuple_item pp8 x8;
        pp_tuple_item pp9 x9;
      ]

  let pp_tuple10 pp1 pp2 pp3 pp4 pp5 pp6 pp7 pp8 pp9 pp10 p fmt
      (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) =
    pp_tuple p fmt
      [
        pp_tuple_item pp1 x1;
        pp_tuple_item pp2 x2;
        pp_tuple_item pp3 x3;
        pp_tuple_item pp4 x4;
        pp_tuple_item pp5 x5;
        pp_tuple_item pp6 x6;
        pp_tuple_item pp7 x7;
        pp_tuple_item pp8 x8;
        pp_tuple_item pp9 x9;
        pp_tuple_item pp10 x10;
      ]

  let pp_pair = pp_tuple2

  let pp_list (pp_e : 'a t) _ fmt l =
    fprintf fmt "@[<2>[";
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@ ") (pp_e false) fmt l;
    fprintf fmt "@,]@]"

  let pp_seq (pp_e : 'a t) _ fmt s =
    fprintf fmt "@[<2><";
    pp_print_seq ~pp_sep:(fun fmt () -> fprintf fmt ";@ ") (pp_e false) fmt s;
    fprintf fmt "@,>@]"

  let pp_array (pp_e : 'a t) _ fmt a =
    fprintf fmt "@[<2>[|";
    pp_print_seq ~pp_sep:(fun fmt () -> fprintf fmt ";@ ") (pp_e false) fmt (Array.to_seq a);
    fprintf fmt "@,|]@]"

  type pp_field = pp_thunk

  let pp_field name (pp_c : 'a t) c fmt =
    fprintf fmt "@[%s =@ %a@]" name (pp_c false) c

  let pp_record _ fmt fields =
    fprintf fmt "@[<2>{ ";
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@ ") (fun fmt ppf -> ppf fmt) fmt fields;
    fprintf fmt "@ }@]"
end

module Equal = struct
  type 'a t = 'a -> 'a -> bool

  let equal_exn = ( = )
  let equal_unit = Unit.equal
  let equal_bool = Bool.equal
  let equal_int = Int.equal
  let equal_int64 = Int64.equal
  let equal_float = Float.equal
  let equal_char = Char.equal
  let equal_string = String.equal
  let equal_option = Option.equal
  let equal_result eq_o eq_e x y = Result.equal ~ok:eq_o ~error:eq_e x y
  let equal_list = List.equal
  let rec equal_seq eq s1 s2 = (* To support OCaml 4.13 as Seq.equal was added in 4.14 *)
    let open Seq in
    match s1 (), s2 () with
    | Nil, Nil -> true
    | Cons (a, an), Cons (b, bn) when eq a b -> equal_seq eq an bn
    | _ -> false
  let equal_array eq x y = equal_seq eq (Array.to_seq x) (Array.to_seq y)
end
