{ name = "halogen-project"
, dependencies = [ "effect"
                 , "halogen"
                 , "prelude"
                 , "random"
                 , "maybe"
                 , "aff"
                 ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
