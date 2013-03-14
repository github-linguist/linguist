
{shared{

  open Eliom_content
  open Html5.D
  open Eliom_parameter

}}

{server{

  module Example =
    Eliom_registration.App
      (struct
        let application_name = "example"
       end)

  let main =
    Eliom_service.service
      ~path:[]
      ~get_params:unit
      ()

}}

{client{

  let hello_popup () =
    Dom_html.window##alert(Js.string ("Hello Popup!"))

}}

{server{

  let _ =

    Example.register
      ~service:main
      (fun () () ->
        Lwt.return
          (html
             (head (title (pcdata "Hello World of Ocsigen")) [])
             (body [h1 [pcdata "Hello World!"];
                    p [pcdata "Welcome to my first Ocsigen website."];
                    h2 ~a:[a_onclick {{ hello_popup () }}]
                      [pcdata "Click me!"]])))

}}
