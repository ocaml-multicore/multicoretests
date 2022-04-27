open QCheck
open Util

(** parallel STM tests of Buffer *)

(* port from the QCSTM example *)
module BConf =
struct
  type cmd =
    | Contents
    (* To_bytes | Sub | Blit *)
    | Nth of int
    | Length
    | Clear
    | Reset
    | Add_char of char
    (* Add_utf8_uchar | Add_utf_16le_uchar | Add_utf_16be_uchar *)
    | Add_string of string
    (*| Add_bytes of bytes*)
    | Truncate of int
  [@@deriving show { with_path = false }]

  type state = char list (* in reverse *)
  type sut = Buffer.t

  let arb_cmd s =
    QCheck.make ~print:show_cmd
      (Gen.oneof [Gen.return Contents;
                  Gen.map (fun i -> Nth i) Gen.small_nat;
                  Gen.return Length;
                  Gen.return Clear;
                  Gen.return Reset;
                  Gen.map (fun c -> Add_char c) Gen.char;
                  Gen.map (fun s -> Add_string s) (Gen.string);
                  Gen.map (fun i -> Truncate i) (let len = List.length s in
                                                 if len = 0
                                                 then Gen.return 0
                                                 else Gen.int_bound (len - 1));
                 ])
  
  let init_state  = []

  let rev_explode s =
    let chars = ref [] in
    String.iter (fun c -> chars := c::!chars) s;
    !chars

  let explode s = List.rev (rev_explode s)
  
  (* changed *)
  let next_state c s = match c with
    | Contents -> s
    | Nth _ -> s
    | Length -> s
    | Clear -> []
    | Reset -> []
    | Add_char ch -> ch::s
    | Add_string str -> (rev_explode str)@s (*s@(explode str)*)
    | Truncate i ->
      let rec trunc buf n = match buf,n with
        | [],0 -> []
        | [],_ -> []
        | _::_,0 -> []
        | c::cs,_ -> c::trunc cs (n-1) in
      List.rev (trunc (List.rev s) i)
  
  let init_sut () = Buffer.create 16
  let cleanup b   = Buffer.reset b

  let precond c s = match c with
    | Truncate i -> i >= 0 && i <= List.length s
    | _ -> true

  (* added *)
  type res =
    | RContent of string
    | RNth of (char, exn) result
    | RLength of int
    | RClear
    | RReset
    | RAdd_char
    | RAdd_string
    | RTruncate of (unit, exn) result [@@deriving show { with_path = false }]
  
  (* changed *)
  let run c b = match c with
    | Contents       -> RContent (Buffer.contents b)
    | Nth i          -> RNth (Util.protect (Buffer.nth b) i)
    | Length         -> RLength (Buffer.length b)
    | Clear          -> Buffer.clear b; RClear
    | Reset          -> Buffer.reset b; RReset
    | Add_char ch    -> Buffer.add_char b ch; RAdd_char
    | Add_string str -> Buffer.add_string b str; RAdd_string
    | Truncate i     -> RTruncate (Util.protect (Buffer.truncate b) i)
      
  (* added *)
  let postcond c s res = match c, res with
    | Contents, RContent str -> explode str = List.rev s
    | Nth i, RNth r ->
       if i < 0 || i >= List.length s
       then r = Error (Invalid_argument "Buffer.nth")
       else Result.fold ~ok:(fun char -> char = List.nth (List.rev s) i) ~error:(fun _ -> false) r
    | Length, RLength i         -> i = List.length s
    | Clear, RClear             -> true
    | Reset, RReset             -> true
    | Add_char _, RAdd_char     -> true
    | Add_string _, RAdd_string -> true
    | Truncate i, RTruncate r ->
       if i < 0 || i > List.length s
       then r = Error (Invalid_argument "Buffer.truncate")
       else r = Ok ()
    | _, _ -> false
end

module BufferSTM = STM.Make(BConf)

;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main
  (let count = 100 in
   [BufferSTM.agree_test     ~count ~name:"buffer test";         (* this test is expected to succeed *)
    BufferSTM.agree_test_par ~count ~name:"buffer test parallel" (* this test is expected to fail *)])
