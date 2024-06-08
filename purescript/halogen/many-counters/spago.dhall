{ name = "halogen-project"
, dependencies = [ "effect"
                 , "halogen"
                 , "prelude"
                 , "arrays"
                 , "maybe"
                 ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
