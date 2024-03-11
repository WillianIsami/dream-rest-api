let () =
  Dream.run ~port:8000
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/" (fun _ ->
      Dream.json {|{"message": "Testing - main"}|}
    );
  ]