{ name = "halogen-project"
, dependencies = [ "console", "effect", "halogen", "maybe", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
