(*
 * Copyright 2024 Fabian Bergström
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

local

fun filterMap f l = List.foldr (fn (x, xs) => case f x of SOME y => y::xs | NONE => xs) [] l

open Http

in

structure Smelly =
struct

structure Log = Log

structure Http = Http

datatype ('a, 'b) result = Ok of 'a | Error of 'b

type listen_sock = (Socket.passive Socket.stream) INetSock.sock
type active_sock = (Socket.active Socket.stream) INetSock.sock

type http_handler = Request.t -> Response.t

fun textResponse status headers body : Response.t =
  let
    val headers = ("Content-Type", "text/plain")::headers
    val headers = ("Content-Length", Int.toString (String.size body))::headers
  in
    {status = status, headers = headers, body = body}
  end

fun encodeResponse (rep: Response.t) : string =
  let
    val headers = #headers rep
    val headers = ("Server", "Smelly")::headers
    val body = #body rep
    fun encodeHeader (k, v) = k ^ ": " ^ v ^ "\r\n"
  in
    (* FIXME: correcterer version? *)
    String.concat [
      "HTTP/1.0 ", Status.toString (#status rep), "\r\n",
      String.concat (map encodeHeader headers),
      "\r\n",
      body
    ]
  end

datatype slurp_error = NoData | TooMuchData

fun slurpUpTo (pattern: string) (sock: active_sock) : (string, slurp_error) result =
  let
    fun slurp _ 0 = Error TooMuchData
      | slurp acc limit =
      let
        val bytes = Socket.recvVec' (sock, 1024, {peek = true, oob = false})
        val s = Byte.bytesToString bytes
        val ss = Substring.full s
        (* FIXME: what if we get a partial match? *)
        val (prefix, suffix) = Substring.position pattern ss
      in
        case (Substring.isEmpty prefix, Substring.isEmpty suffix) of
          (true, true) => Error NoData
        | (_, true) => slurp (s::acc) (limit - 1)
        | _ =>
            let
              val discardCount = Substring.size prefix
              val last = Socket.recvVec (sock, discardCount)
              val slurped = String.concat (List.rev (Byte.bytesToString last::acc))
            in
              Ok slurped
            end
      end
  in
    slurp [] 8
  end

(* FIXME: body *)
fun slurpRequest (sock: active_sock) : (Request.t, Status.t) result =
  case slurpUpTo "\n" sock of
    Error _ => Error Status.BadRequest
  | Ok reqLine =>
      case slurpUpTo "\r\n\r\n" sock of
        Error _ => Error Status.BadRequest
      | Ok headersString =>
          case Request.parse reqLine headersString of
            NONE => Error Status.BadRequest
          | SOME req => Ok req

fun respond (sock: active_sock) (rep: Response.t) =
  let
    val str = encodeResponse rep
    val bytes = Byte.stringToBytes str
    fun pushBytes _ 0 = ()
      | pushBytes bytes n =
        let
          val m = Socket.sendVec (sock, bytes)
          val bytes = Word8VectorSlice.subslice (bytes, m, NONE)
        in
          pushBytes bytes (n - m)
        end
  in
    pushBytes (Word8VectorSlice.full bytes) (Word8Vector.length bytes)
  end

fun handleClient (sock: active_sock) (handler: http_handler) =
  let
    val req = slurpRequest sock
  in
    case req of
      Error e => respond sock (Response.mk e [] "")
    | Ok req => respond sock (handler req)
    ;
    Socket.close sock
  end
  handle e => Log.error (exnMessage e)

fun serve (sock: listen_sock) (handler: http_handler) : unit =
  let
    val (clientSock, _) = Socket.accept sock
  in
    handleClient clientSock handler;
    serve sock handler
  end

end

end
