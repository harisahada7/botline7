open Lwt
open Cohttp_lwt_unix
open Cohttp
open Yojson.Basic.Util
open Types
let _ = Curl.global_init Curl.CURLINIT_GLOBALALL

(* [user_environment] is the environments for all users 
 * containing any defintions that the user has created *)
let user_environments = Hashtbl.create 10

(* [get_env sender_psid] gets the environment for user with id [sender_psid] *)
 let get_env sender_psid : Types.value Types.PMap.t =
  match Hashtbl.find_opt user_environments sender_psid with
  | Some env -> env
  | None -> Hashtbl.add user_environments sender_psid (
    PMap.empty
    |> PMap.add "'prime" (E "'prime has not be not bound")
    |> PMap.add "'p" (E "'p has not be not bound")
    |> PMap.add "'q" (E "'q has not be not bound")
    |> PMap.add "'n" (E "'n has not be not bound")
    |> PMap.add "'d" (E "'d has not be not bound")
    |> PMap.add "'e" (E "'e has not be not bound")
    |> PMap.add "'prime_prob" (E "'prime_prob has not be not bound")
  );
  Hashtbl.find user_environments sender_psid

(**************************************************************************
 * Following code found on https://github.com/RamV13/ohttp
 * Used for creating API endpoints called by Facebook when message is sent
 * to the chat bot
***************************************************************************)

type meth = Code.meth
type uri = Uri.t

type request = {
                 headers : Header.t;
                 params : (string * string) list;
                 req_body : string
               }

type response = {
                  headers : Header.t;
                  status : Code.status_code;
                  res_body : string
                }

type callback = request -> response

type custom_callback = request -> (Response.t * Cohttp_lwt.Body.t) Lwt.t

type route = (meth * uri) * callback

type t = route list

let server = ref []

let add_route (meth,uri) callback =
  let server_callback req =
    let res = callback req in
    Server.respond ~headers:res.headers ~status:res.status
                   ~body:(Cohttp_lwt.Body.of_string res.res_body) ()
  in
  server := ((meth,uri),server_callback)::!server

let add_custom_route (meth,uri) custom_callback =
  server := ((meth,uri),custom_callback)::!server

let callback _ req body =
  let meth = Request.meth req in
  let uri = req |> Request.uri in
  try
    let headers = Request.headers req in
    let params =
      List.map (fun query -> ((fst query),List.hd (snd query))) (Uri.query uri)
    in
    body
    |> Cohttp_lwt.Body.to_string
    >>= fun req_body ->
        begin
          {headers;params;req_body}
          |> List.assoc (meth,Uri.path uri) !server
        end
  with Not_found -> Server.respond_string ~status:`Not_found ~body:"" ()

(**************************************************************************
 * Following code found on https://gist.github.com/zbroyar/1432555
 * Originally tried to also use OHttp developed by Ram for POST requests but it
 * wouldn't work no documentation could be found online for Cohttp. 
 * This was the only code available that could succesfully make 
 * a POST request to Facebook.
***************************************************************************)

let writer_callback a d =
	Buffer.add_string a d;
	String.length d

let init_conn url =
	let r = Buffer.create 16384
	and c = Curl.init () in
	Curl.set_timeout c 1200;
	Curl.set_sslverifypeer c false;
	Curl.set_sslverifyhost c Curl.SSLVERIFYHOST_EXISTENCE;
	Curl.set_writefunction c (writer_callback r);
	Curl.set_tcpnodelay c true;
	Curl.set_verbose c false;
	Curl.set_post c false;
	Curl.set_url c url; r,c

let post ?(content_type = "application/json") url data =
  print_endline ("Request body: " ^ data);
  let r,c = init_conn url in
  Curl.set_post c true;
  Curl.set_httpheader c [ "Content-Type: " ^ content_type ];
  Curl.set_postfields c data;
  Curl.set_postfieldsize c (String.length data);
  Curl.perform c;
  let rc = Curl.get_responsecode c in
  Curl.cleanup c;
  rc, (Buffer.contents r)
(**************************************************************************)


(* helper function to create body of Facebook API request *)
let formulate_body sender_psid response = 
  "{
    \"recipient\" : {
      \"id\" : \"" ^ sender_psid ^
    "\"},
    \"message\":" ^ response ^
  "}"

let callSendAPI sender_psid response =
  let request_body = formulate_body sender_psid response in
  let post_url = "https://graph.facebook.com/v2.6/me/messages?access_token=EAAEZBhqyWObQBAED8CndCr1WRaFMTjCwdF1qfLb78CXt3G15ZC6POeaaSjPzUiY8ve9by9PJk2OmJs7P8daeqFQz6Bj05MKhWNgmiJJFyyr8fzuZAh3G8gIZBzkvOO6UFXBio1Yf4oLZAoCuOLC3ZBMsEXqo94LOyhB0kl2wtzmDyFUSyZAj7nv" in

  (* calls post request helper function with formatted data *)
  post post_url request_body


(* [webhook req] is called by Facebook 
 * messenger when a message is sent to Calculator bot. *)
let webhook req =
  let headers = Header.init_with "Content-Type" "application/json" in
  let j = Yojson.Basic.from_string req.req_body in

  (* [entries] is the list of messages sent by facebook to process by calculator *)
  let entries =  j |> member "entry" |> to_list in
  let status = `OK in

  (* [handle entry] handles and then responds to entry, a single message sent by user to chat bot *)
  let handle_entry entry = (
    let webhook_event = List.nth (entry |> member "messaging" |> to_list) 0 in
    let sender_psid = webhook_event |> member "sender" |> member "id" |> to_string in

    (* logs message sent by user for server debugging *)
    print_endline ("Message: " ^ (webhook_event |> member "message" |> Yojson.Basic.to_string));

    (* gets command from request body *)
    let command = webhook_event |> member "message" |> member "text" |> to_string in
    let lower_command = command |> String.lowercase_ascii in

    (* determines response to command and updates in hash table
     * to environment produced by running said command *)
    let (result, env') = (Eval.evaluate_line (get_env sender_psid) command) in
    (Hashtbl.replace user_environments sender_psid env';

    (* formats new line characters for post request body *)
    let regex = Str.regexp "\n" in
    let escaped = (result  |> Str.global_replace regex "\\n") in

    (* builds json to match Facebook API schema*)
    let message = ("{\"text\": \"" ^ escaped ^ "\"}") in

    (* send calculator response to Facebook for printing in messenger*)
    callSendAPI sender_psid message)
  ) in 
    
  (* uses to List.map to handle each message contained in [entries]*)
  List.map handle_entry entries;

  (* formulates reponse to api request made by Facebook *)
  let res_body = "Processed request successfully" in
    {headers; status; res_body}
    
    
(* endpoint used to verify webhook is running *)
let webhook_verif req =
  let headers = Header.init_with "Content-Type" "text/plain" in
  let verify_token = "sdfkjsflkgjassodofiwpoeirhskxmcnbmdfsldjf" in
    if List.mem_assoc "hub.mode" req.params
    && List.mem_assoc "hub.verify_token" req.params
    && List.mem_assoc "hub.challenge" req.params
    then
      let mode = List.assoc "hub.mode" req.params in
      let token = List.assoc "hub.verify_token" req.params in
      let challenge = List.assoc "hub.challenge" req.params in

      if mode = "subscribe" && token = verify_token then
        (print_string "WEBHOOK VERIFIED!!!!!!";
        let status = `OK in
        let res_body = challenge in
          {headers; status; res_body})
      else
        let status = `Unauthorized in
        let res_body = "sad times" in
          {headers; status; res_body}
    else
      let status = `Unauthorized in
      let res_body = "sad times" in
        {headers; status; res_body}

let run ?(port=8000) _ =
  Lwt.async_exception_hook := ignore;
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())
  |> Lwt_main.run
  |> ignore

let _ =
  add_route (`POST, "/webhook") webhook;
  add_route (`GET, "/webhook") webhook_verif;
  run ~port:1337 ()



