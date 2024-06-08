{ name = "halogen-project"
, dependencies = [ "effect"
                 , "halogen"
                 , "prelude"
                 , "random"
                 , "maybe"
                 , "aff"
                 , "either"
                 , "web-events"
                 , "affjax"
                 , "affjax-web"
                 ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
