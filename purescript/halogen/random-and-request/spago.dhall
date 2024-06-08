{ name = "halogen-project"
, dependencies = [ "effect"
                 , "halogen"
                 , "prelude"
                 , "random"
                 , "maybe"
                 ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
