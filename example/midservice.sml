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

structure Log = Smelly.Log

fun eq a b = a = b

datatype greet_style = Loud | Quiet | Default

fun stringToUpper s = String.implode (List.map Char.toUpper (String.explode s))

fun greet Loud (name: string) = "HELLO, " ^ (stringToUpper name) ^ "!!!\n"
  | greet Default (name: string) = "Hello, " ^ name ^ "!\n"
  | greet Quiet (name: string) = "hi " ^ name ^ "\n"

fun greeter (params: (string*string) list) (name: string) =
  case List.find (fn (k, _) => k = "style") params of
    NONE => Smelly.textResponse Http.StatusCode.OK [] (greet Default name)
  | SOME (_, "loud") => Smelly.textResponse Http.StatusCode.OK [] (greet Loud name)
  | SOME (_, "quiet") => Smelly.textResponse Http.StatusCode.OK [] (greet Quiet name)
  | _ => Smelly.textResponse Http.StatusCode.BadRequest [] "Bad style\n"

fun router (req: Smelly.request) =
  let
    val method = Smelly.method req
    val path = String.tokens (eq #"/") (Smelly.path req)
    val params = Smelly.parseQuery req
  in
    case (method, path) of
      (Http.Request.GET, []) => greeter params "world"
    | (Http.Request.GET, name::_) => greeter params name
    | _ => Smelly.textResponse Http.StatusCode.NotFound [] "Not found\n"
  end

fun main () =
  let
    val _ = Log.setLevel Log.INFO
    val sock = INetSock.TCP.socket () : Smelly.listen_sock
    val portOpt = Option.mapPartial Int.fromString (OS.Process.getEnv "PORT")
    val port =
      case portOpt of
        NONE => 3000
      | SOME x => x
  in
    Socket.Ctl.setREUSEADDR (sock, true);
    Socket.bind (sock, INetSock.any port);
    Socket.listen (sock, 5);
    Log.info ("Listening on port " ^ (Int.toString port));
    Smelly.serve sock router
  end
  handle e => (
    Log.error (General.exnMessage e);
    OS.Process.exit OS.Process.failure
  )
