{-
   Run with:
      elm-cli run src/Hello.elm

   Compile with:
      elm-cli make src/Hello.elm hello-world
-}


module Hello exposing (program)

import Dict
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Proc


lazy : a -> () -> a
lazy f () =
    f


printFile : String -> IO (Result String ())
printFile fileName =
    let
        doPrint : Result err String -> IO (Result err ())
        doPrint res =
            case res of
                Err e ->
                    IO.return (Err e)

                Ok content ->
                    Proc.print content
                        |> IO.map (lazy Ok ())
    in
    File.contentsOf fileName
        |> IO.andThen doPrint


printEnv : Process -> String -> IO (Result String ())
printEnv process name =
    case Dict.get name process.env of
        Nothing ->
            IO.return (Err "no file")

        Just val ->
            IO.map Ok (Proc.print val)



{- This also works
   printEnv : Process -> String -> IO (Result String ())
   printEnv process name =
       Dict.get name process.env
           |> Maybe.map (IO.map Ok << Proc.print)
           |> Maybe.withDefault (IO.return (Err "oops"))
-}


otherProgram : Process -> IO ()
otherProgram process =
    IO.map (always ()) <|
        IO.combine
            [ printFile "hello.txt" |> IO.exitOnError identity
            , printFile "world.txt" |> IO.exitOnError identity
            , printEnv process "HOME" |> IO.exitOnError (always "HOME not found")
            , printEnv process "USER" |> IO.exitOnError (always "USER not found")
            ]


program : Process -> IO ()
program process =
    let
        mustPrintFile : String -> IO ()
        mustPrintFile =
            printFile >> IO.exitOnError identity

        mustPrintEnv : String -> IO ()
        mustPrintEnv env =
            printEnv process env
                |> IO.exitOnError (always <| env ++ " not found")
    in
    IO.return ()
        |> IO.andThen (mustPrintFile "hello.txt" |> lazy)
        |> IO.andThen (mustPrintFile "world.txt" |> lazy)
        |> IO.andThen (mustPrintEnv "HOME" |> lazy)
        |> IO.andThen (mustPrintEnv "USER" |> lazy)
        |> IO.andThen (otherProgram process |> lazy)
