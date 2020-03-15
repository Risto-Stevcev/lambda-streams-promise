open LambdaStreams

let first_to_promise stream =
  Js.Promise.make (fun ~resolve ~reject:_ ->
      stream
      |> Async.listen (function
             | Signal.Data value -> ( resolve value [@bs])
             | _ -> ()))

let last_to_promise stream =
  let latest_value = ref None in
  Js.Promise.make (fun ~resolve ~reject:_ ->
      stream
      |> Async.listen (fun value ->
             match value, !latest_value with
             | Signal.Data value, _ -> latest_value := Some value
             | EndOfSignal, Some value -> ( resolve value [@bs])
             | _ -> ()))

let from_promise promise =
  Async.make @@ fun cb ->
  promise
  |> Js.Promise.then_ (fun value ->
         cb @@ Signal.Data value;
         cb EndOfSignal;
         Js.Promise.resolve ())
  |> ignore
