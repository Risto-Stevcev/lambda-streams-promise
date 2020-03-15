open LambdaStreams

val first_to_promise : 'a Finite.Async.t -> 'a Js.Promise.t

val last_to_promise : 'a Finite.Async.t -> 'a Js.Promise.t

val from_promise : 'a Js.Promise.t -> 'a Finite.Async.t
