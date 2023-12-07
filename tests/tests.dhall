let Tool = < GHC | Cabal >

let Variable =
  < Version : { tool: Tool, versions: List Text } >

let Test =
  { testPath: Text, tool: Tool, args: List Text, variables: List Variable }

let ghcVersions = { tool = Tool.GHC, versions = [ "8.10.7", "9.8.1" ] }

let ghc_build_helloWorld =
  { testPath  = "./tests/hello"
  , tool      = Tool.GHC
  , args      = [ "Hello.hs" ]
  , variables = [ Variable.Version ghcVersions ]
  }

let tests = [ { name = "ghc_build_helloWorld", test = ghc_build_helloWorld } ]

in tests
