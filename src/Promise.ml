open LambdaStreams

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
