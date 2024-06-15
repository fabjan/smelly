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

(*
 * A simplish HTTP service.
 *)

fun method (req: MyHttp.request) : Http.Request.method =
  #method (#line req)

fun path (req: MyHttp.request) : string =
  case #uri (#line req) of
      Http.Uri.PATH p => #path p
    | Http.Uri.URL u => #path u
    | _ => "/"

fun eq a b = a = b

fun pushBytes _ _ 0 = ()
  | pushBytes sock bytes n =
    let
      val m = Socket.sendVec (sock, bytes)
      val bytes = Word8VectorSlice.subslice (bytes, m, NONE)
    in
      pushBytes sock bytes (n - m)
    end

fun respond (req: MyHttp.request) status contentType body =
  let
    val headers = [
      ("Server", "MyHttp"),
      ("Content-Type", contentType),
      ("Content-Length", Int.toString (String.size body))
    ]
    val response = MyHttp.mkResponse status headers body
    val str = MyHttp.encodeResponse response
    val bytes = Byte.stringToBytes str
    val sock = #sock req
  in
    pushBytes sock (Word8VectorSlice.full bytes) (Word8Vector.length bytes)
  end

fun router (req: MyHttp.request) =
  case (method req, String.tokens (eq #"/") (path req)) of
      (Http.Request.GET, []) => respond req Http.StatusCode.OK "text/plain" "Hello, World!\n"
    | _ => respond req Http.StatusCode.NotFound "text/plain" "Not found\n"

fun main () =
  let
    val _ = Log.setLevel Log.INFO
    val sock = INetSock.TCP.socket () : MyHttp.listen_sock
    val portOpt = Option.mapPartial Int.fromString (OS.Process.getEnv "PORT")
    val port = case portOpt of
        NONE => 3000
      | SOME x => x
  in
    Socket.Ctl.setREUSEADDR (sock, true);
    Socket.bind (sock, INetSock.any port);
    Socket.listen (sock, 5);
    Log.info ("Listening on port " ^ (Int.toString port));
    MyHttp.serve sock router
  end
