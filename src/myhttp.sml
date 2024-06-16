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

structure MyHttp =
struct

type listen_sock = (Socket.passive Socket.stream) INetSock.sock
type active_sock = (Socket.active Socket.stream) INetSock.sock

type request = {
  line: Http.Request.line,
  headers: (string * string) list,
  sock: active_sock
}

type response = {
  status: Http.StatusCode.t,
  headers: (string * string) list,
  body: string
}

fun mkResponse status headers body : response =
  {status = status, headers = headers, body = body}

fun encodeResponse (response: response) =
  let
    val line = {version = Http.Version.HTTP_1_0, status = #status response}
    val headers = #headers response
    val body = #body response
  in
    Http.Response.toString {line = line, headers = headers, body = SOME body}
  end

fun slurpUpTo (pattern: string) (sock: active_sock) =
  let
    fun slurp _ 0 = raise Fail "too much header data"
      | slurp acc tries =
      let
        val bytes = Socket.recvVec' (sock, 1024, {peek = true, oob = false})
        val s = Byte.bytesToString bytes
        val _ = Log.debug ("slurping: " ^ s)
        val ss = Substring.full s
        val (prefix, suffix) = Substring.position pattern ss
      in
        case (Substring.isEmpty prefix, Substring.isEmpty suffix) of
          (true, true) => raise Fail ("pattern not found: " ^ (String.toString pattern))
        | (_, true) => slurp (s::acc) (tries - 1)
        | _ =>
            let
              val discardCount = Substring.size prefix
              val _ = Socket.recvVec (sock, discardCount)
            in
              (Substring.full o String.concat o List.rev) (s::acc)
            end
      end
  in
    slurp [] 8
  end

fun parseRequest (sock: active_sock) : (request, string) result =
  case Http.Request.parse_line Substring.getc (slurpUpTo "\n" sock) of
    NONE => Error "cannot parse request line"
  | SOME (line, _) =>
      let
        val headers = Http.Request.parse_headers Substring.getc (slurpUpTo "\r\n\r\n" sock)
        val headers = Option.map #1 headers
        val headers = fromOpt [] headers
      in
        Ok {line = line, headers = headers, sock = sock}
      end

fun serve (sock: listen_sock) handler =
  let
    val _ = Log.debug "Waiting for connection..."
    val (clientSock, _) = Socket.accept sock
  in
    Log.debug "Connection accepted, parsing request";
    (case parseRequest clientSock of
      Error e => Log.error e
    | Ok req =>
        handler req;
    Socket.close clientSock)
    handle e => Log.error (exnMessage e);
    serve sock handler
  end


end
