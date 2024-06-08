{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "halogen"
  , "halogen-subscriptions"
  , "maybe"
  , "prelude"
  , "random"
  , "strings"
  , "tailrec"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
