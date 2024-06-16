## Smelly

Smelly is probably one of the top ten* HTTP server framework for Standard ML.

![nose](nose.jpeg)

```sml
structure Log = Smelly.Log

fun router (req: Smelly.request) =
  let
    val method = Smelly.method req
    val path = String.tokens (fn c => c = #"/") (Smelly.path req)
  in
    case (method, path) of
      (Http.Request.GET, []) => Smelly.textResponse Http.StatusCode.OK [] "Hello, World!\n"
    | (Http.Request.GET, name::_) => Smelly.textResponse Http.StatusCode.OK [] ("Hello, " ^ name ^ "!\n")
    | _ => Smelly.textResponse Http.StatusCode.NotFound [] "Not found\n"
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
