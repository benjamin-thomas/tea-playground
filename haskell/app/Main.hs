{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Main (main) where

import Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as DTC
import System.IO
  ( BufferMode (NoBuffering)
  , hSetBuffering
  , hSetEcho
  , stdin
  )
import System.Random (randomRIO)
import Prelude hiding (init)

data Model = Model
  { mCount :: Int
  , mLastRandomValue :: Maybe Int
  , mMessage :: Maybe String
  }

init :: (Model, Cmd Action)
init =
  ( Model
      { mCount = 0
      , mLastRandomValue = Nothing
      , mMessage = Nothing
      }
  , Cmd []
  )

-- MESSAGE

data UserInput
  = IncPressed
  | DecPressed
  | ResetPressed
  | RequestRandomPressed
  | RequestTimePressed
  | RequestRandomAndGetTimePressed
  | ClearScreenPressed

data Action
  = UserInputAction UserInput
  | GotRandomNumber Int
  | GotTime UTCTime

-- UPDATE

update :: Model -> Action -> (Model, Cmd Action)
update model = \case
  UserInputAction userInput ->
    handleUserInput model userInput
  GotRandomNumber n ->
    ( model
        { mLastRandomValue = Just n
        , mMessage = Just $ "Random number generated: " <> show n
        }
    , cmdNone
    )
  GotTime time ->
    ( model
        { mMessage = Just $ "Current time: " <> show time
        }
    , cmdNone
    )

handleUserInput :: Model -> UserInput -> (Model, Cmd Action)
handleUserInput model userInput =
  case userInput of
    IncPressed ->
      ( model
          { mCount = mCount model + 1
          }
      , cmdNone
      )
    DecPressed ->
      ( model
          { mCount = mCount model - 1
          }
      , cmdNone
      )
    ResetPressed ->
      ( model
          { mCount = 0
          , mMessage = Just "Counter reset to zero"
          }
      , cmdNone
      )
    RequestRandomPressed ->
      ( model
          { mMessage = Just "Generating random number..."
          }
      , generateRandomNumber
      )
    RequestTimePressed ->
      ( model
          { mMessage = Just "Getting current time..."
          }
      , getCurrentTime
      )
    RequestRandomAndGetTimePressed ->
      ( model
          { mMessage =
              Just "Generating random number and getting current time..."
          }
      , batch
          [ generateRandomNumber
          , getCurrentTime
          ]
      )
    ClearScreenPressed ->
      ( model
          { mMessage = Nothing
          }
      , cmdNone
      )

-- VIEW

data ConsoleUI
  = Line String
  | BlankLine
  | Section [ConsoleUI]
  | Divider Char Int

view :: Model -> ConsoleUI
view model =
  Section
    [ Divider '-' 50
    , Line $ "Current count: " ++ show (mCount model)
    , case mLastRandomValue model of
        Just n -> Line $ "Last random value: " ++ show n
        Nothing -> Line "No random value generated yet"
    , case mMessage model of
        Just action -> Line $ "Message: " <> action
        Nothing -> BlankLine
    , BlankLine
    , Line "Commands:"
    , Line "  i - Increment"
    , Line "  d - Decrement"
    , Line "  r - Reset"
    , Line "  g - Generate random number"
    , Line "  t - Get current time"
    , Line "  b - Get a random number AND get the current time (both)"
    , Line "  c - Clear message"
    , Line "  q - Quit"
    , Divider '-' 50
    ]

-- SUBSCRIPTIONS (interpret user input)

inputToAction :: Char -> Maybe Action
inputToAction c = case c of
  'i' -> Just $ UserInputAction IncPressed
  'd' -> Just $ UserInputAction DecPressed
  'r' -> Just $ UserInputAction ResetPressed
  'g' -> Just $ UserInputAction RequestRandomPressed
  't' -> Just $ UserInputAction RequestTimePressed
  'b' -> Just $ UserInputAction RequestRandomAndGetTimePressed
  'c' -> Just $ UserInputAction ClearScreenPressed
  _ -> Nothing

-------------------------------------------------------------------------------
--
-- NOTE: Anything above this point is pure         => (does not access IO)
-- NOTE: Anything below this point may be impure   =>      (may access IO)
--
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

batch :: [Cmd action] -> Cmd action
batch cmds = Cmd (concatMap (\(Cmd ios) -> ios) cmds)

runCmd :: Model -> Cmd Action -> IO Model
runCmd model (Cmd ios) =
  foldl applyUpdate model <$> sequence ios

applyUpdate :: Model -> Action -> Model
applyUpdate model action = fst $ update model action

-------------------------------------------------------------------------------
--
-- USER AVAILABLE COMMANDS
--
-------------------------------------------------------------------------------

generateRandomNumber :: Cmd Action
generateRandomNumber = Cmd [GotRandomNumber <$> randomRIO (1, 100)]

getCurrentTime :: Cmd Action
getCurrentTime = Cmd [GotTime <$> DTC.getCurrentTime]

-------------------------------------------------------------------------------
--
-- RENDERING
--
-------------------------------------------------------------------------------

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

renderConsoleUI :: ConsoleUI -> IO ()
renderConsoleUI ui = case ui of
  Line str -> putStrLn str
  BlankLine -> putStrLn ""
  Section uis -> mapM_ renderConsoleUI uis
  Divider char n -> putStrLn (replicate n char)

-------------------------------------------------------------------------------
--
-- RUNTIME
--
-------------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering -- Don't buffer key inputs
  hSetEcho stdin False -- Don't echo characters
  let (initModel, initCmd) = init
  newModel <- runCmd initModel initCmd
  runLoop newModel
 where
  runLoop :: Model -> IO ()
  runLoop model = do
    clearScreen
    renderConsoleUI (view model)

    input <- getChar
    if input == 'q'
      then
        putStrLn "\nGoodbye!"
      else do
        case inputToAction input of
          Nothing ->
            runLoop model
          Just action -> do
            let (newModel, newCmd) = update model action
            runLoop =<< runCmd newModel newCmd
