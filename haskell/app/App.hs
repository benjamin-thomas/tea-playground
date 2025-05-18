module App (program) where

import TEA
  ( Cmd
  , ConsoleUI
    ( BlankLine
    , Divider
    , Line
    , Section
    )
  , Program
    ( MkProgram
    , cInit
    , cInputToAction
    , cUpdate
    , cView
    )
  , TeaTime
  , cmdBatch
  , cmdNone
  , requestRandomNumber
  , requestTime
  )
import TEA.Prelude

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
  , cmdNone
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
  | GotTime TeaTime

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

getRandomNumber :: Cmd Action
getRandomNumber = requestRandomNumber GotRandomNumber

getTime :: Cmd Action
getTime = requestTime GotTime

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
      , getRandomNumber
      )
    RequestTimePressed ->
      ( model
          { mMessage = Just "Getting current time..."
          }
      , getTime
      )
    RequestRandomAndGetTimePressed ->
      ( model
          { mMessage =
              Just "Generating random number and getting current time..."
          }
      , cmdBatch
          [ getRandomNumber
          , getTime
          ]
      )
    ClearScreenPressed ->
      ( model
          { mMessage = Nothing
          }
      , cmdNone
      )

-- VIEW

view :: Model -> ConsoleUI
view model =
  Section
    [ Divider '-' 50
    , Line $ "Current count: " <> show (mCount model)
    , case mLastRandomValue model of
        Just n -> Line $ "Last random value: " <> show n
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

program :: Program Model Action
program =
  MkProgram
    { cInit = init
    , cUpdate = update
    , cView = view
    , cInputToAction = inputToAction
    }
