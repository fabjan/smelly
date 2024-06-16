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

fun unHex #"0" = SOME 0
  | unHex #"1" = SOME 1
  | unHex #"2" = SOME 2
  | unHex #"3" = SOME 3
  | unHex #"4" = SOME 4
  | unHex #"5" = SOME 5
  | unHex #"6" = SOME 6
  | unHex #"7" = SOME 7
  | unHex #"8" = SOME 8
  | unHex #"9" = SOME 9
  | unHex #"a" = SOME 10
  | unHex #"b" = SOME 11
  | unHex #"c" = SOME 12
  | unHex #"d" = SOME 13
  | unHex #"e" = SOME 14
  | unHex #"f" = SOME 15
  | unHex #"A" = SOME 10
  | unHex #"B" = SOME 11
  | unHex #"C" = SOME 12
  | unHex #"D" = SOME 13
  | unHex #"E" = SOME 14
  | unHex #"F" = SOME 15
  | unHex _ = NONE

fun urlDecode (s: string): string option =
  let
    fun isAllowed c = Char.isAlphaNum c orelse c = #"_" orelse c = #"." orelse c = #"-" orelse c = #"~"
    fun loop (ss: Substring.substring) (acc: string list) =
      case Substring.getc ss of
        NONE => SOME (String.concat (rev acc))
      | SOME (#"%", rest) =>
          (case Substring.getc rest of
            NONE => NONE
          | SOME (c1, more) =>
              (case Substring.getc more of
                NONE => NONE
              | SOME (c2, continue) =>
                  (case (unHex c1, unHex c2) of
                    (SOME n1, SOME n2) => loop continue (Char.toString (Char.chr (n1 * 16 + n2))::acc)
                  | _ => NONE
                  )
              )
          )
      | SOME (#"+", rest) => loop rest (" "::acc)
      | SOME (c, rest) =>
          if not (isAllowed c)
          then NONE
          else loop rest (Char.toString c::acc)
  in
    loop (Substring.full s) []
  end

in

structure Http =
struct

structure Status =
struct
  type t = int
  val Ok = 200
  val BadRequest = 400
  val NotFound = 404
  val InternalServerError = 500

  fun toString (status: t) : string =
    case status of
      200 => "200 OK"
    | 400 => "400 Bad Request"
    | 404 => "404 Not Found"
    | 500 => "500 Internal Server Error"
    | _ => "Unknown"
end

structure Method =
struct
  (* Four methods ought to be enough for anyone. *)
  datatype t = Get | Post | Put | Delete

  fun toString Get = "GET"
    | toString Post = "POST"
    | toString Put = "PUT"
    | toString Delete = "DELETE"

end

local

fun parseRequestLine (s: string) =
  case String.fields (fn c => c = #" ") s of
    ["GET", uri, version] => SOME {method = Method.Get, uri = uri, version = version}
  | ["POST", uri, version] => SOME {method = Method.Post, uri = uri, version = version}
  | ["PUT", uri, version] => SOME {method = Method.Put, uri = uri, version = version}
  | ["DELETE", uri, version] => SOME {method = Method.Delete, uri = uri, version = version}
  | _ => NONE

fun parseUri (s: string) =
  case String.tokens (fn c => c = #"?") s of
    [path, query] => SOME (path, query)
  | [path] => SOME (path, "")
  | _ => NONE

fun parseHeaders (s: string) =
  let
    val lines = String.tokens (fn c => c = #"\n") s
    val lines = filterMap (fn line =>
      case String.fields (fn c => c = #":") line of
        [k, v] => SOME (k, v)
      | _ => NONE
    ) lines
  in
    lines
  end

in

structure Request =
struct
  type t = {
    method: Method.t,
    path: string,
    query: (string * string) list,
    headers: (string * string) list
  }
  (* FIXME: this is a bit naive *)
  fun parse (reqLine: string) (headers: string) : t option =
    case parseRequestLine reqLine of
      NONE => NONE
    | SOME line =>
        case parseUri (#uri line) of
          NONE => NONE
        | SOME (path, query) =>
            let
              val method = #method line : Method.t
              val params = String.tokens (fn c => c = #"&") query
              val pairs = List.map (fn s => String.tokens (fn c => c = #"=") s) params
              fun decodePair (k, v) =
                case (urlDecode k, urlDecode v) of
                  (SOME k', SOME v') => SOME (k', v')
                | _ => NONE
              fun parse (k::v::[]) = decodePair (k, v)
                | parse [k] = decodePair (k, "")
                | parse _ = NONE
              val query = filterMap parse pairs
              val headers = parseHeaders headers
            in
              SOME {method = method, path = path, query = query, headers = headers}
            end

  fun param (req: t) (key: string) : string option =
    case List.find (fn (k, _) => k = key) (#query req) of
      NONE => NONE
    | SOME (_, v) => SOME v
end

end

structure Response =
struct
  type t = {
    status: Status.t,
    headers: (string * string) list,
    (* FIXME: binary? *)
    body: string
  }

  fun mk (status: Status.t) (headers: (string * string) list) (body: string) : t =
    {status = status, headers = headers, body = body}
end

end

end
