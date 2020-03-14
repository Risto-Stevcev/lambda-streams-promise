open LambdaStreams

module Interval = Async.Interval (struct
  type interval_id = Js.Global.intervalId

  let set_interval = Js.Global.setInterval

  let clear_interval = Js.Global.clearInterval
end)

let first_to_promise (stream : 'a Finite.Async.t) : 'a Js.Promise.t =
  Js.Promise.make (fun ~resolve ~reject:_ ->
      stream
      |> Async.listen (function
             | Signal.Data value -> ( resolve value [@bs])
             | _ -> ()))

let last_to_promise (stream : 'a Finite.Async.t) : 'a Js.Promise.t =
  let latest_value = ref None in
  Js.Promise.make (fun ~resolve ~reject:_ ->
      stream
      |> Async.listen (fun value ->
             match value, !latest_value with
             | Signal.Data value, _ -> latest_value := Some value
             | EndOfSignal, Some value -> ( resolve value [@bs])
             | _ -> ()))

let from_promise (promise : 'a Js.Promise.t) : 'a Finite.Async.t =
  Async.make @@ fun cb ->
  promise
  |> Js.Promise.then_ (fun value ->
         cb @@ Signal.Data value;
         cb EndOfSignal;
         Js.Promise.resolve ())
  |> ignore

let _ =
  let { Connection.stream; close } = Interval.make ~ms:1500 in
  stream |> Finite.Async.take' ~close 5 |> Finite.Async.map (fun e -> Js.log e) |> first_to_promise
