open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type message_object = {
  message: string;
} [@@deriving yojson]

let render param = 
  <html>
    <body>
      <p> The message is <%s param %> </p>
    </body>
  </html>

let my_error_template _error _ suggested_response =
  let status = Dream.status suggested_response in
  let code = Dream.status_to_int status
  and reason = Dream.status_to_string status in

  Dream.set_header suggested_response "Content-Type" Dream.text_html;
  Dream.set_body suggested_response begin
    <html>
    <body>
      <h1><%i code %> <%s reason %></h1>
    </body>
    </html>
  end;
  Lwt.return suggested_response



let () =
  Dream.run ~error_handler:(Dream.error_template my_error_template)
  @@ Dream.logger
  @@ Dream.origin_referrer_check
  @@ Dream.router [
    
    Dream.get "/send/:word" (fun request -> 
      Dream.param request "word"
      |> render 
      |> Dream.html);

    Dream.get "/fail"
      (fun _ ->
        raise (Failure "The Web app failed!"));
    
    Dream.get "/bad" (fun _ -> 
      Dream.empty `Bad_Request);

    Dream.post "/send" (fun request -> 
      let%lwt body = Dream.body request in 
      Dream.respond
        ~headers:["Content-Type", "application/json"] body);

    Dream.post "/content" (fun request -> 
      let%lwt body = Dream.body request in 
      
      let message_object =  
        body 
        |> Yojson.Safe.from_string
        |> message_object_of_yojson
      in 

      `String message_object.message
      |> Yojson.Safe.to_string
      |> Dream.json);
  ]