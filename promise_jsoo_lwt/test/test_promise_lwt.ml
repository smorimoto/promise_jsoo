open Webtest.Suite

open
  [%js:
  val setTimeout : (unit -> unit) -> int -> unit [@@js.global]

  val eval : string -> Ojs.t [@@js.global]]

let pass finish () = finish Async.noop

let fail finish () = finish (fun () -> assert_true false)

let timeout f = setTimeout f 1000

let valid promise = Option.is_some @@ [%js.to: 'a option] promise

module LwtConversion = struct
  let test_of_promise_fulfilled finish =
    let js_promise = Promise.return 1 in
    let lwt_promise = Promise_lwt.of_promise js_promise in
    let fulfilled value = finish (fun () -> assert_equal value 1) in
    let rejected _ = fail finish () in
    Lwt.on_any lwt_promise fulfilled rejected;
    timeout (fail finish)

  let test_of_promise_rejected finish =
    let js_promise = Promise.reject @@ [%js.of: int] 2 in
    let lwt_promise = Promise_lwt.of_promise js_promise in
    let fulfilled _ = fail finish () in
    let rejected = function
      | Promise_lwt.Promise_error reason ->
        let reason = [%js.to: int] reason in
        finish (fun () -> assert_equal reason 2)
      | _ -> fail finish ()
    in
    Lwt.on_any lwt_promise fulfilled rejected;
    timeout (fail finish)

  let test_of_promise_throw finish =
    let js_promise =
      Promise.make (fun ~resolve:_ ~reject:_ -> ignore @@ eval "throw 3")
    in
    let lwt_promise = Promise_lwt.of_promise js_promise in
    let fulfilled _ = fail finish () in
    let rejected = function
      | Promise_lwt.Promise_error reason ->
        let reason = [%js.to: int] reason in
        finish (fun () -> assert_equal reason 3)
      | _ -> fail finish ()
    in
    Lwt.on_any lwt_promise fulfilled rejected;
    timeout (fail finish)

  let test_to_promise_fulfilled finish =
    let lwt_promise, resolver = Lwt.wait () in
    Lwt.wakeup_later resolver 1;
    let js_promise = Promise_lwt.to_promise lwt_promise in
    let fulfilled value =
      finish (fun () -> assert_equal value 1);
      Promise.return ()
    in
    let rejected _ =
      fail finish ();
      Promise.return ()
    in
    let (_ : unit Promise.t) = Promise.then_ js_promise ~fulfilled ~rejected in
    timeout (fail finish)

  let test_to_promise_rejected_exn finish =
    let exception E of int in
    let lwt_promise, resolver = Lwt.wait () in
    Lwt.wakeup_later_exn resolver (E 2);
    let js_promise = Promise_lwt.to_promise lwt_promise in
    let fulfilled _ =
      fail finish ();
      Promise.return ()
    in
    let rejected (reason : Promise.error) =
      let reason : exn = Obj.magic reason in
      finish (fun () -> assert_equal reason (E 2));
      Promise.return ()
    in
    let (_ : unit Promise.t) = Promise.then_ js_promise ~fulfilled ~rejected in
    timeout (fail finish)

  let test_to_promise_rejected_error finish =
    let js_promise_1 = Promise.reject @@ [%js.of: int] 3 in
    let lwt_promise = Promise_lwt.of_promise js_promise_1 in
    let js_promise_2 = Promise_lwt.to_promise lwt_promise in
    let fulfilled _ =
      fail finish ();
      Promise.return ()
    in
    let rejected (reason : Promise.error) =
      let reason = [%js.to: int] reason in
      finish (fun () -> assert_equal reason 3);
      Promise.return ()
    in
    let (_ : unit Promise.t) =
      Promise.then_ js_promise_2 ~fulfilled ~rejected
    in
    timeout (fail finish)

  let test_to_promise_raise finish =
    let exception E of int in
    let lwt_promise = Lwt.wrap (fun () -> raise (E 4)) in
    let js_promise = Promise_lwt.to_promise lwt_promise in
    let fulfilled _ =
      fail finish ();
      Promise.return ()
    in
    let rejected (reason : Promise.error) =
      let reason : exn = Obj.magic reason in
      finish (fun () -> assert_equal reason (E 4));
      Promise.return ()
    in
    let (_ : unit Promise.t) = Promise.then_ js_promise ~fulfilled ~rejected in
    timeout (fail finish)

  let suite =
    "LwtConversion"
    >::: [ "test_of_promise_fulfilled" >:~ test_of_promise_fulfilled
         ; "test_of_promise_rejected" >:~ test_of_promise_rejected
         ; "test_of_promise_throw" >:~ test_of_promise_throw
         ; "test_to_promise_fulfilled" >:~ test_to_promise_fulfilled
         ; "test_to_promise_rejected_exn" >:~ test_to_promise_rejected_exn
         ; "test_to_promise_rejected_error" >:~ test_to_promise_rejected_error
         ; "test_to_promise_raise" >:~ test_to_promise_raise
         ]
end

let suite = "Promise" >::: [ LwtConversion.suite ]

let () = Webtest_js.Runner.run suite
