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
structure Http = Smelly.Http

local

open Http

fun eq a b = a = b

datatype style = Loud | Default | Quiet

fun stringToUpper s = String.implode (List.map Char.toUpper (String.explode s))

fun greet Loud (name: string) = "HELLO, " ^ (stringToUpper name) ^ "!!!\n"
  | greet Default (name: string) = "Hello, " ^ name ^ "!\n"
  | greet Quiet (name: string) = "hi " ^ name ^ "\n"

fun stringToStyle s =
  case s of
    "loud" => SOME Loud
  | "quiet" => SOME Quiet
  | _ => NONE

fun greeter style (SOME extra) name = greeter style NONE (name ^ "-" ^ extra)
  | greeter NONE NONE name = Smelly.textResponse Status.Ok [] (greet Default name)
  | greeter (SOME s) NONE name =
      case stringToStyle s of
        SOME style => Smelly.textResponse Status.Ok [] (greet style name)
      | NONE => Smelly.textResponse Status.BadRequest [] ("Bad style: " ^ s ^ "\n")

fun router (req: Request.t) =
  let
    val method = #method req
    val path = String.tokens (eq #"/") (#path req)
    val style = Http.Request.param req "style"
    val extra = Http.Request.param req "  "
  in
    case (method, path) of
      (Method.Get, []) => greeter style extra "world"
    | (Method.Get, name::_) => greeter style extra name
    | _ => Smelly.textResponse Status.NotFound [] "Not found\n"
  end

in

fun main () =
  let
    val _ = Log.setLevel Log.INFO
    val sock = INetSock.TCP.socket () : Smelly.listen_sock
    val portOpt = Option.mapPartial Int.fromString (OS.Process.getEnv "PORT")
    val queueOpt = Option.mapPartial Int.fromString (OS.Process.getEnv "QUEUE_LENGTH")
    val port = Option.getOpt (portOpt, 3000)
    val queue = Option.getOpt (queueOpt, 5)
  in
    Socket.Ctl.setREUSEADDR (sock, true);
    Socket.bind (sock, INetSock.any port);
    Socket.listen (sock, queue);
    Log.info ("Listening on port " ^ (Int.toString port));
    Smelly.serve sock router
  end
  handle e => (
    Log.error (General.exnMessage e);
    OS.Process.exit OS.Process.failure
  )

end
