(*
    npm install
    tab1: dotnet watch fable
    tab2: npx vite
*)

open Elmish
open Elmish.React
open Main

let () =
    Program.mkSimple init update view
    |> Program.withReactSynchronous "root"
    |> Program.run
