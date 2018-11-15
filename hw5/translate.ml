(*
 * SNU 4190.310 Programming Languages 
 * K-- to SM5 translator skeleton code
 *)

open K
open Sm5
module Translator = struct
  (* TODO : complete this function  *)
  let gen_id =
    let id = ref 0 in
    fun prefix ->
      let new_id = !id in
      id := new_id + 1;
      prefix ^ "@" ^ string_of_int new_id
  let scoped name cmds =
    [Sm5.MALLOC; Sm5.BIND name; Sm5.PUSH (Sm5.Id name); Sm5.STORE] @
    cmds @
    [Sm5.UNBIND; Sm5.POP]
  let loop var econd ebody =
    let loop_name = gen_id "#loop" in
    K.LETF (loop_name, var,
            K.IF (econd,
                  K.SEQ(ebody, K.CALLR(loop_name, var)),
                  K.UNIT),
            K.CALLR(loop_name, var))
  let rec trans : K.program -> Sm5.command =
    function
    | K.NUM i -> [Sm5.PUSH (Sm5.Val (Sm5.Z i))]
    | K.TRUE -> [Sm5.PUSH (Sm5.Val (Sm5.B true))]
    | K.FALSE -> [Sm5.PUSH (Sm5.Val (Sm5.B false))]
    | K.UNIT -> [Sm5.PUSH (Sm5.Val (Sm5.Unit))]
    | K.VAR id -> [Sm5.PUSH (Sm5.Id id); Sm5.LOAD]
    | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [Sm5.ADD]
    | K.SUB (e1, e2) -> trans e1 @ trans e2 @ [Sm5.SUB]
    | K.MUL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.MUL]
    | K.DIV (e1, e2) -> trans e1 @ trans e2 @ [Sm5.DIV]
    | K.EQUAL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.EQ]
    | K.LESS (e1, e2) -> trans e1 @ trans e2 @ [Sm5.LESS]
    | K.NOT e -> trans e @ [Sm5.NOT]
    | K.ASSIGN (id, exp) ->
      trans exp @ [Sm5.PUSH (Sm5.Id id); Sm5.STORE;] @ trans (K.VAR id)
    | K.SEQ (e1, e2) -> trans e1 @ [Sm5.POP] @ trans e2
    | K.IF (econd, e1, e2) -> trans econd @ [Sm5.JTR (trans e1, trans e2)]
    | K.LETV (x, e1, e2) ->
      trans e1 @ scoped x (trans e2)
    | K.LETF (func, param, fexpr, expr) ->
      (* Add epilogue, and prologue for recursive calling.
         Caller must provide this function as is. *)
      let fcmd = [Sm5.BIND func] @ trans fexpr @ [Sm5.UNBIND; Sm5.POP] in
      [Sm5.PUSH (Sm5.Fn (param, fcmd)); Sm5.BIND func] @
      trans expr @
      [Sm5.UNBIND; Sm5.POP]
    | K.CALLV (fname, arg) ->
      let arg_name = gen_id "#callv_var" in
      trans arg @ scoped arg_name (
        (* for callee's epilogue *)
        [Sm5.PUSH (Sm5.Id fname)] @
        (* callee function *)
        [Sm5.PUSH (Sm5.Id fname)] @
        (* callee argument value *)
        [Sm5.PUSH (Sm5.Id arg_name); Sm5.LOAD] @
        (* callee argument location *)
        [Sm5.MALLOC] @
        [Sm5.CALL]
      )
    | K.CALLR (fname, x) ->
      (* for callee's epilogue *)
      [Sm5.PUSH (Sm5.Id fname)] @
      (* callee function *)
      [Sm5.PUSH (Sm5.Id fname)] @
      (* ugly hack. we need a location to pass, but Sm5.CALL requires a value,
         so existing value itself is provided, to maintain an identity. *)
      [Sm5.PUSH (Sm5.Id x); Sm5.LOAD] @
      (* callee argument location *)
      [Sm5.PUSH (Sm5.Id x)] @
      [Sm5.CALL]
    | K.READ x -> [Sm5.GET; Sm5.PUSH (Sm5.Id x); Sm5.STORE;] @ trans (K.VAR x)
    | K.WRITE e ->
      let var_name = gen_id "#write_var" in
      (* eval & temporary bind & assign *)
      trans e @ scoped var_name (
        (* load & put*)
        trans (K.VAR var_name) @ [Sm5.PUT] @
        (* load for return*)
        trans (K.VAR var_name)
      )
    | K.WHILE (econd, ebody) ->
      let dummy_var = "#_" in
      trans (K.LETV(dummy_var, K.TRUE, loop dummy_var econd ebody))
    | K.FOR (id, e1, e2, ebody) ->
      let counter = gen_id "#for_counter" in
      let left = gen_id "#for_left" in
      let countdown = gen_id "#for_countdown" in
      let loop_body =
        loop id
          (K.NOT(K.LESS(K.VAR(countdown), K.NUM 0)))
          (K.SEQ(
              K.ASSIGN(id, K.ADD(K.VAR(left), K.VAR(counter))),
              K.SEQ(
                ebody,
                K.SEQ(
                  K.ASSIGN(counter, K.ADD(K.VAR(counter), K.NUM 1)),
                  K.ASSIGN(countdown, K.SUB(K.VAR(countdown), K.NUM 1))))))
      in
      trans (
        K.LETV(left, e1,
               K.LETV(countdown, K.SUB(e2, K.VAR(left)),
                      K.LETV(counter, K.NUM 0,
                             loop_body))))
end
