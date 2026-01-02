/// Test entry point for Fidelity.Toml tests.
module Fidelity.Toml.Tests.Main

open Expecto

[<EntryPoint>]
let main args =
    runTestsInAssemblyWithCLIArgs [] args
