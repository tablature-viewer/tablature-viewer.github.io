{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "arrays"
  , "assert"
  , "barlow-lens"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "enums"
  , "exists"
  , "filterable"
  , "foldable-traversable"
  , "foreign"
  , "functors"
  , "halogen"
  , "http-methods"
  , "integers"
  , "js-timers"
  , "lists"
  , "maybe"
  , "newtype"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "quickcheck"
  , "record"
  , "simple-json"
  , "string-parsers"
  , "strings"
  , "stringutils"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
