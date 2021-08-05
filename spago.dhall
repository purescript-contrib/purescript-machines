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
  , "psci-support"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "doc-test/*.purs" ]
}
