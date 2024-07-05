module Routes where

import Prelude

import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Routing.Match (Match, end, int, lit, root)

type PostId = Int
data Route
  = PostIndex
  | PostShow PostId
  | PostEdit PostId
  | ButtonR
  | RootR
  | NotFound

derive instance Eq Route
derive instance Generic Route _

instance Show Route where
  show = genericShow

routes :: Match Route
routes =
  oneOf
    [ pure RootR <$> root <* end
    , postRoutes
    , pure ButtonR <$> (root *> lit "button" <* end)
    , pure NotFound
    ]

postRoutes :: Match Route
postRoutes = root *> lit "posts" *> oneOf
  [ PostIndex <$ end
  , PostShow <$> int <* end
  , PostEdit <$> int <* lit "edit" <* end
  ]