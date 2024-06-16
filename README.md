## Smelly

Smelly is probably one of the top ten* HTTP server frameworks for Standard ML.

![nose](nose.jpeg)

```sml
structure Log = Smelly.Log
structure Http = Smelly.Http

fun router (req: Http.Request.t) =
  let
    val method = #method req
    val path = String.tokens (fn c => c = #"/") (#path req)
  in
    case (method, path) of
      (Http.Method.Get, []) => Smelly.textResponse Http.Status.Ok [] "Hello, World!\n"
    | (Http.Method.Get, name::_) => Smelly.textResponse Http.Status.Ok [] ("Hello, " ^ name ^ "!\n")
    | _ => Smelly.textResponse Http.Status.NotFound [] "Not found\n"
  end

fun main () =
  let
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
```

\*please open an issue if/when this number needs bumping
