(**************************************************************************)
(*                                                                        *)
(*                        OCamlPro Typerex                                *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the LGPL v3.0            *)
(*   (GNU Lesser General Public Licence version 3.0).                     *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)

(*
TODO:
 * [exit] is not implemented
 * [Process.create] with stdin redirection is not implemented
*)


let _ =
  Sys.signal Sys.sigpipe (Sys.Signal_handle (fun _ -> ()))

open StringCompat

module OldPrintf = Printf
module OldUnix = Unix
(* open Core.Std *)
open Async.Std
module Socket = Async.Std.Unix.Socket

module Unix = OldUnix
module Printf = OldPrintf

(* A simple client-server *)

let debug = false

type 'info connection = {
  info : 'info;
  sock : ([ `Active],  Socket.Address.Inet.t) Socket.t;
  reader : Reader.t;
  mutable writer : Writer.t option;
}

let info con = con.info

let should_exit = ref false
let main () =
  Core.Std.never_returns (Scheduler.go ())

let exit () =
  Async.Std.don't_wait_for (exit 0)

(* Let's try to be efficient about buffers *)
module BytesCache : sig

  (* [get n] gets a buffer of at least [n] bytes. The final length [m]
     such that [m >= n] will be a power of 2. *)
  val get : int -> bytes

  (* [putback b] when [b] will not be used anymore *)
  val putback : bytes -> unit

end = struct

  let find_slot n =
    let rec find_slot n i =
      if n = 0 then i else
        find_slot (n/2) (i+1)
    in
    find_slot n 0

  let buffers = Array.make 64 []

  let get n =
    let i = find_slot (n-1) in
    let m = 1 lsl i in
    match buffers.(i) with
    | hd :: tl ->
      buffers.(i) <- tl;
      assert (Bytes.length hd >= n);
      hd
    | [] -> Bytes.create m

  let putback b =
    let n = Bytes.length b in
    if n >= 4096 then
      let i = find_slot (n-1) in
      if List.length buffers.(i) < 4 then
        buffers.(i) <- b :: buffers.(i)

end

let send_message con msg =
  if debug then Printf.eprintf "send_message...\n%!";
  match con.writer with
  | None -> ()
  | Some writer ->
    let msg_len = String.length msg in
    let total_msg_len = 4+msg_len in
    let b = BytesCache.get total_msg_len in
    EndianString.LittleEndian.set_int32 b 0 (Int32.of_int msg_len);
    Bytes.blit msg 0 b 4 msg_len;
    if not (Writer.is_closed writer) then
      Writer.write writer b ~pos:0 ~len:total_msg_len

let shutdown con =
  match con.writer with
  | None -> ()
  | Some writer ->
    Socket.shutdown  con.sock `Both

let close con =
  match con.writer with
  | None -> ()
  | Some writer ->
    Async.Std.don't_wait_for (Writer.close writer);
    con.writer <- None;
    Async.Std.don't_wait_for (Fd.close (Socket.fd con.sock))

module MakeSocket(S : sig

  type server_info
  type info

  val connection_info : server_info -> Unix.sockaddr -> info

  val connection_handler : info connection -> unit
  val message_handler : info connection -> string -> unit
  val disconnection_handler : info -> unit

end) = (struct

  let disconnection_handler con =
    try
      S.disconnection_handler con
    with exn ->
      Printf.eprintf "Warning: [disconnection_handler] raised exception %s\n%!"
        (Printexc.to_string exn)

  let connection_handler con =
    try
      S.connection_handler con
    with exn ->
      Printf.eprintf "Warning: [connection_handler] raised exception %s\n%!"
        (Printexc.to_string exn)

  let message_handler con msg =
    try
      S.message_handler con msg
    with exn ->
      Printf.eprintf "Warning: [message_handler] raised exception %s\n%!"
        (Printexc.to_string exn)

  let rec iter_read con b pos =
    (* Printf.eprintf "\titer_read %d...\n%!" pos; *)
    let blen = Bytes.length b in
    let can_read = blen - pos in
    if can_read < 16384 then begin
      let newb = BytesCache.get (4 * blen) in
      Bytes.blit b 0 newb 0 pos;
      BytesCache.putback b;
      iter_read con newb pos;
    end
    else
    if not (Reader.is_closed con.reader) then
      Deferred.bind (Reader.read con.reader b ~pos ~len:can_read)
        (function
          | `Eof ->
            BytesCache.putback b;
            disconnection_handler con.info;
            Async.Std.return ()
          | `Ok nr ->
            (* Printf.eprintf "\titer_read %d/%d...\n%!" nr pos; *)
            iter_parse con b nr pos
        )
    else begin
      BytesCache.putback b;
      disconnection_handler con.info;
      Async.Std.return ()
    end

  and iter_parse con b nr pos =
      (* Printf.eprintf "\titer_parse %d %d\n%!" nr pos; *)
    let pos = pos + nr in
    if pos > 4 then
      let msg_len = Int32.to_int
        (EndianString.LittleEndian.get_int32 b 0) in
        (* Printf.eprintf "\tmsg_len=%d\n" msg_len; *)
      let total_msg_len = msg_len + 4 in
      if total_msg_len > pos then
        iter_read con b pos
      else
        let msg = Bytes.sub b 4 msg_len in
        message_handler con msg;
        let nr = pos - total_msg_len in
        if nr > 0 then begin
          Bytes.blit b total_msg_len b 0 nr;
          iter_parse con b nr 0
        end else
          iter_read con b 0
    else
      iter_read con b pos

  let raise_when_consumer_leaves = false

  let create ~loopback ?(port=0) context =
    let module Unix = OldUnix in
    let sockaddr = Unix.ADDR_INET(
      (if loopback then
          Unix.inet_addr_of_string "127.0.0.1"
       else
          Unix.inet_addr_any),
      port) in

    let sock = Socket.create Socket.Type.tcp in
    let fd = Socket.fd sock in
    let ufd = Fd.file_descr_exn fd in
    Unix.setsockopt ufd Unix.SO_REUSEADDR true;
    Unix.bind ufd sockaddr;
    Unix.listen ufd 20;
    let s = Socket.of_fd fd Socket.Type.tcp in
    Async.Std.don't_wait_for
      (
        let rec iter_accept () =
          Deferred.bind (Socket.accept s)
            (* TODO: check that we can receive multiple connections !!! *)
            (function
            | `Socket_closed ->
              iter_accept ()
            | `Ok (sock, inet) ->
              let sock_addr =
                Unix.ADDR_INET (
                  Socket.Address.Inet.addr inet,
                  Socket.Address.Inet.port inet) in
              (* Printf.eprintf "\tServer received connection...\n%!"; *)
              let b = BytesCache.get (1 lsl 16) in
              let info = S.connection_info context sock_addr in
              let fd = Socket.fd sock in
              let writer = Some (Writer.create fd ~raise_when_consumer_leaves) in
              let reader = Reader.create fd in
              let con = { info; sock; reader; writer } in
              connection_handler con;
              Async.Std.don't_wait_for (iter_read con b 0);
              iter_accept ())
        in
        iter_accept ()
      );
    let port = match Unix.getsockname ufd with
        Unix.ADDR_INET(_, port) -> port
      | _ -> assert false in
    port

  let create_server = create

  let connect info sockaddr =
    let (addr, port) =
      match sockaddr with
        Unix.ADDR_INET (addr, port) -> (addr, port)
      | _ -> assert false
    in
    let sock = Socket.create Socket.Type.tcp in
    let fd = Socket.fd sock in
    let reader = Reader.create (Socket.fd sock) in
    let writer = Some (Writer.create fd ~raise_when_consumer_leaves) in
    let con =
      let sock = Socket.of_fd fd Socket.Type.tcp in
      { info; sock; reader; writer } in
    Async.Std.don't_wait_for
      (Deferred.bind
         (Monitor.try_with ~extract_exn:true
            (fun () ->
               Socket.connect sock (Socket.Address.Inet.create addr ~port)
            ))
         (function
           | Core.Std.Result.Ok sock ->
             if debug then Printf.eprintf "Connected\n%!";
             connection_handler con;
             let b = Bytes.create 65636 in
             iter_read con b 0;
           | Core.Std.Result.Error exn ->
             Printf.eprintf "Exception during connect: %s\n%!"
               (Printexc.to_string exn);
             disconnection_handler info;
             Deferred.return ()
         ));
    con

end : sig

  val create : loopback:bool -> ?port:int -> S.server_info -> int
  val create_server : loopback:bool -> ?port:int -> S.server_info -> int
  val connect : S.info -> Unix.sockaddr -> S.info connection

end)


(* create sockets as server *)
module MakeServer(S : sig

  type server_info
  type info

  val connection_info : server_info -> Unix.sockaddr -> info

  (* [connection_handler sock ] *)
  val connection_handler : info connection -> unit

  (* [message_handler conn_id sock msg_id msg_content] *)
  val message_handler : info connection -> string -> unit

  (* [disconnection_handler conn_id] *)
  val disconnection_handler : info -> unit

end) = MakeSocket(S)

(* create sockets as client *)
module MakeClient(S : sig

  type info

  (* [connection_handler sock ] *)
  val connection_handler : info connection -> unit

  (* [message_handler conn_id sock msg_id msg_content] *)
  val message_handler : info connection -> string -> unit

  (* [disconnection_handler conn_id] *)
  val disconnection_handler : info -> unit

end) = MakeSocket(struct
  type server_info = S.info
  include S
  let connection_info _sockaddr = assert false
end
)


let exec prog args ?timeout ?stdin ?stdout  ?stderr cont =
  (* TODO: should Async args contain the command, or shall we remove it ? *)
  let args = Array.to_list args in
  let stdin = match stdin with
    | None -> None
    | Some filename ->
      let fd = Unix.openfile filename [Unix.O_RDONLY] 0o644 in
      Some (`FD_move fd)
  in
  match stdin with
  | None ->
    Async.Std.don't_wait_for (
      Deferred.bind
        (Process.create ~prog ~args ())
        (function
            Core_kernel.Result.Ok ps ->
            Deferred.bind (Process.wait ps)
              (fun o ->

                 let status = match o.Async.Std.Process.Output.exit_status with
                   | Core_kernel.Std.Result.Ok () -> Unix.WEXITED 0
                   | Core_kernel.Std.Result.Error (`Exit_non_zero n) -> Unix.WEXITED n
                   | Core_kernel.Std.Result.Error (`Signal _) -> Unix.WEXITED 98
                 in
                 let call_status () =
                   cont status;
                   Deferred.return ()
                 in
                 match stdout,stderr with
                 | None, None -> call_status ()
                 | Some stdout_file, None ->
                   Deferred.bind
                     (Writer.save stdout_file
                        o.Async.Std.Process.Output.stdout)
                     call_status
                 | None, Some stderr_file ->
                   Deferred.bind
                     (Writer.save stderr_file
                        o.Async.Std.Process.Output.stderr)
                     call_status
                 | Some stdout_file, Some stderr_file ->
                   Deferred.bind
                     (Writer.save stdout_file
                        o.Async.Std.Process.Output.stdout)
                     (fun () ->
                        Deferred.bind
                          (Writer.save stderr_file
                             o.Async.Std.Process.Output.stderr)
                          call_status)
              )
          | Core_kernel.Result.Error error ->
            cont (Unix.WEXITED 99);
            Deferred.return ()
        ))
  | _ ->
    assert false (* For now, there is nothing to do that... *)


module Timer = struct

  type t = bool ref

  let create n f =
    let t = ref true in
    Async.Std.upon
      (Clock.after (Core.Std.sec (float_of_int n)))
      (fun () -> if !t then f ());
    t

  let stop t = t := false
end
