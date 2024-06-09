{ name = "halogen-project"
, dependencies =
  [ "effect"
  , "halogen"
  , "maybe"
  , "prelude"
  , "tuples"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
