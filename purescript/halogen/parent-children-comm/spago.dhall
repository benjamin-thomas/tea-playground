{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "console"
  , "datetime"
  , "effect"
  , "halogen"
  , "halogen-subscriptions"
  , "maybe"
  , "prelude"
  , "tailrec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
