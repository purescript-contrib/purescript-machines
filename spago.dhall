{ name = "machines"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "effect"
  , "lists"
  , "maybe"
  , "prelude"
  , "profunctor"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
