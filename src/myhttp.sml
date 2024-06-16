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
  (* FIXME: binary? *)
  body: string
}

fun mkResponse status headers body : response =
  {status = status, headers = headers, body = body}

fun encodeResponse (response: response) : string =
  let
    val line = {version = Http.Version.HTTP_1_0, status = #status response}
    val headers = #headers response
    val body = #body response
  in
    Http.Response.toString {line = line, headers = headers, body = SOME body}
  end

datatype slurp_error = NoData | TooMuchData

fun slurpUpTo (pattern: string) (sock: active_sock) : (Substring.substring, slurp_error) result =
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
              val _ = Socket.recvVec (sock, discardCount)
              val slurped = String.concat (List.rev (s::acc))
            in
              Ok (Substring.full slurped)
            end
      end
  in
    slurp [] 8
  end

fun parseRequest (sock: active_sock) : (request, Http.StatusCode.t) result =
  case slurpUpTo "\n" sock of
    Error _ => Error Http.StatusCode.BadRequest
  | Ok reqLine =>
      case Http.Request.parse_line Substring.getc reqLine of
        NONE => Error Http.StatusCode.BadRequest
      | SOME (line, _) =>
          case slurpUpTo "\r\n\r\n" sock of
            Error _ => Error Http.StatusCode.BadRequest
          | Ok headers =>
              let
                val headers = Http.Request.parse_headers Substring.getc headers
                val headers = Option.map #1 headers
                val headers = fromOpt [] headers
              in
                Ok {line = line, headers = headers, sock = sock}
              end

fun serve (sock: listen_sock) handler : unit =
  let
    val _ = Log.debug "Waiting for connection..."
    val (clientSock, _) = Socket.accept sock
  in
    Log.debug "Connection accepted, parsing request";
    (case parseRequest clientSock of
      Error e => Log.error (Http.StatusCode.toString e)
    | Ok req =>
        handler req;
    Socket.close clientSock)
    handle e => Log.error (exnMessage e);
    serve sock handler
  end

end
