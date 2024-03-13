open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module type DB = Caqti_lwt.CONNECTION
module T = Caqti_type

let list_tasks = 
  let query = 
    let open Caqti_request.Infix in 
    (T.unit ->* T.(tup2 int string ))
    "SELECT id, text FROM task" in 
  fun (module Db: DB) -> 
    let%lwt tasks_or_error = Db.collect_list query () in 
    Caqti_lwt.or_fail tasks_or_error 
  
let add_task = 
  let query = 
    let open Caqti_request.Infix in 
    (T.string ->. T.unit)
    "INSERT INTO task (text) VALUES ($1)" in 
  fun text ( module Db : DB) -> 
    let%lwt unit_or_error = Db.exec query text in
    Caqti_lwt.or_fail unit_or_error 

let render tasks request = 
  <html>
  <body>

%   tasks |> List.iter (fun (_id, task) -> 
      <p><%s task %></p><% ); %>
    
    <form method="POST" action="/task">
      <%s! Dream.csrf_tag request %>
      <input name="text" autofocus>
    </form>

  </body>
  </html>

type message_object = {
  message: string;
} [@@deriving yojson]

let render_message param = 
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
  @@ Dream.sql_pool "sqlite3:db.sqlite"
  @@ Dream.sql_sessions
  @@ Dream.router [
    
    Dream.get "/" (fun request -> 
      let%lwt tasks = Dream.sql request list_tasks in 
      Dream.html (render tasks request));

    Dream.post "/task" (fun request -> 
      match%lwt Dream.form request with 
      | `Ok ["text", text] -> 
        let%lwt () = Dream.sql request (add_task text) in 
        Dream.redirect request "/"
      | _ -> 
        Dream.empty `Bad_Request);
    
    Dream.get "/send/:word" (fun request -> 
      Dream.param request "word"
      |> render_message 
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