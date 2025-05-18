module TEA
    ( start
    , Program
        ( MkProgram
        , cInit
        , cUpdate
        , cView
        , cInputToAction
        )
    , Cmd
    , cmdNone
    , cmdBatch
    , TeaTime
    , ConsoleUI
        ( Line
        , BlankLine
        , Section
        , Divider
        )
    , requestTime
    , requestRandomNumber
    ) where

import Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as DTC
import System.IO
    ( BufferMode (NoBuffering)
    , hSetBuffering
    , hSetEcho
    , stdin
    )
import System.Random (Random, randomRIO)

import Prelude hiding (init)

-------------------------------------------------------------------------------
--
-- RENDERING
--
-------------------------------------------------------------------------------

data ConsoleUI
    = Line String
    | BlankLine
    | Section [ConsoleUI]
    | Divider Char Int

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- renderConsoleUI :: ConsoleUI -> IO ()
renderConsoleUI :: ConsoleUI -> IO ()
renderConsoleUI ui =
    clearScreen >>= \() ->
        _render ui

-- _render :: ConsoleUI -> IO ()
_render :: ConsoleUI -> IO ()
_render ui = case ui of
    Line str -> putStrLn str
    BlankLine -> putStrLn ""
    Section uis -> mapM_ _render uis
    Divider char n -> putStrLn (replicate n char)

-------------------------------------------------------------------------------
--
-- RUNTIME
--
-------------------------------------------------------------------------------

data Program model action
    = MkProgram
    { cInit :: (model, Cmd action)
    , cUpdate :: model -> action -> (model, Cmd action)
    , cView :: model -> ConsoleUI
    , cInputToAction :: Char -> Maybe action
    }

setupConsole :: IO ()
setupConsole = do
    hSetBuffering stdin NoBuffering -- Don't buffer key inputs
    hSetEcho stdin False -- Don't echo characters

-- start :: IO ()
start :: Program model action -> IO ()
start cfg = do
    setupConsole
    let (initModel, initCmd) = cInit cfg
    newModel <- runCmd (cUpdate cfg) initModel initCmd
    runLoop cfg newModel

runLoop :: Program model action -> model -> IO ()
runLoop cfg model = do
    let viewF = cView cfg
    let inputToActionF = cInputToAction cfg
    let consoleUI = viewF model :: ConsoleUI
    renderConsoleUI consoleUI

    input <- getChar
    if input == 'q'
        then
            putStrLn "\nGoodbye!"
        else do
            case inputToActionF input of
                Nothing ->
                    runLoop cfg model
                Just action -> do
                    let (newModel, newCmd) = cUpdate cfg model action
                    runLoop cfg =<< runCmd (cUpdate cfg) newModel newCmd

-------------------------------------------------------------------------------
--
-- COMMAND INTERNALS
--
-------------------------------------------------------------------------------

-- Cmd represents side effects that will be executed by a runtime
-- You "command" the runtime to perform these actions
newtype Cmd action = Cmd [IO action]

-- Helper functions to create commands
cmdNone :: Cmd action
cmdNone = Cmd []

cmdBatch :: [Cmd action] -> Cmd action
cmdBatch cmds = Cmd (concatMap (\(Cmd ios) -> ios) cmds)

runCmd :: (model -> action -> (model, Cmd action)) -> model -> Cmd action -> IO model
runCmd updateF model (Cmd ios) =
    foldl (applyUpdate updateF) model <$> sequence ios

applyUpdate :: (model -> action -> (model, Cmd action)) -> model -> action -> model
applyUpdate updateF model action = fst $ updateF model action

-------------------------------------------------------------------------------
--
-- USER AVAILABLE COMMANDS
--
-------------------------------------------------------------------------------

requestRandomNumber :: (Random a, Num a) => (a -> action) -> Cmd action
requestRandomNumber toAction = Cmd [toAction <$> randomRIO (1, 100)]

newtype TeaTime = TeaTime UTCTime

toTeaTime :: UTCTime -> TeaTime
toTeaTime = TeaTime

instance Show TeaTime where
    show (TeaTime time) = show time

requestTime :: (TeaTime -> action) -> Cmd action
requestTime toAction = Cmd [toAction . toTeaTime <$> DTC.getCurrentTime]