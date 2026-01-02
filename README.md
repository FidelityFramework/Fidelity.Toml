# Fidelity.Toml

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![TOML](https://img.shields.io/badge/TOML-1.0.0-orange.svg)](https://toml.io/en/v1.0.0)

A fully TOML 1.0.0 compliant parser for F#, built with XParsec.

## Overview

Fidelity.Toml is a pure F# implementation of a [TOML 1.0.0](https://toml.io/en/v1.0.0) parser using parser combinators. It is designed to be:

- **Spec-compliant**: Full TOML 1.0.0 support including all data types, datetime formats, and structural features
- **Pure F#**: No external parsing dependencies beyond XParsec
- **Self-hosting ready**: Part of the Fidelity Framework toolchain infrastructure

## Part of the Fidelity Framework

This library is a component of the [Fidelity Framework](https://github.com/FidelityFramework) - an ecosystem for compiling F# to native code without the .NET runtime.

### Why a Separate TOML Library?

Fidelity.Toml exists as a standalone library because:

1. **Self-hosting path**: As the Fidelity toolchain matures toward self-hosting, every dependency must be expressible in native F#. Having a pure F# TOML parser (using XParsec) ensures the entire toolchain can eventually compile itself.

2. **Shared infrastructure**: Both the Firefly compiler and FsNativeAutoComplete (the LSP server) need to parse `.fidproj` project files. A shared library eliminates duplication.

3. **General utility**: A compliant TOML parser is useful beyond project files - configuration, data exchange, and more.

4. **Joy**: [XParsec](https://github.com/roboz0r/XParsec) is a great library and I really dig working with it.

## Installation

```bash
dotnet add package Fidelity.Toml
```

Or add to your `.fsproj`:

```xml
<PackageReference Include="Fidelity.Toml" Version="0.1.0" />
```

## Usage

```fsharp
open Fidelity.Toml

// Parse a TOML string
let toml = """
[package]
name = "my-project"
version = "1.0.0"

[build]
sources = ["Main.fs", "Lib.fs"]
output = "myapp"
"""

match Toml.parse toml with
| Ok document ->
    // Access values
    let name = Toml.getString "package.name" document  // Some "my-project"
    let sources = Toml.getStringArray "build.sources" document  // Some ["Main.fs"; "Lib.fs"]
    printfn "Project: %A" name

| Error msg ->
    eprintfn "Parse error: %s" msg
```

## Supported TOML Features

### Data Types

| Type | Example | Status |
|------|---------|--------|
| String (basic) | `"hello\nworld"` | Supported |
| String (literal) | `'C:\path\to\file'` | Supported |
| String (multiline) | `"""..."""` | Supported |
| Integer (decimal) | `42`, `1_000_000` | Supported |
| Integer (hex) | `0xDEADBEEF` | Supported |
| Integer (octal) | `0o755` | Supported |
| Integer (binary) | `0b11010110` | Supported |
| Float | `3.14`, `5e10`, `inf`, `nan` | Supported |
| Boolean | `true`, `false` | Supported |
| Offset Date-Time | `1979-05-27T07:32:00Z` | Supported |
| Local Date-Time | `1979-05-27T07:32:00` | Supported |
| Local Date | `1979-05-27` | Supported |
| Local Time | `07:32:00` | Supported |

### Structural Features

| Feature | Example | Status |
|---------|---------|--------|
| Tables | `[section]` | Supported |
| Nested Tables | `[section.subsection]` | Supported |
| Inline Tables | `point = { x = 1, y = 2 }` | Supported |
| Arrays | `ports = [80, 443]` | Supported |
| Array of Tables | `[[products]]` | Supported |
| Dotted Keys | `physical.color = "red"` | Supported |
| Comments | `# comment` | Supported |

## API Reference

### Parsing

```fsharp
/// Parse a TOML string into a document
val parse : string -> Result<TomlDocument, string>
```

### Value Access

```fsharp
/// Get a string value by dotted key path
val getString : string -> TomlDocument -> string option

/// Get an integer value
val getInt : string -> TomlDocument -> int64 option

/// Get a float value
val getFloat : string -> TomlDocument -> float option

/// Get a boolean value
val getBool : string -> TomlDocument -> bool option

/// Get an array of strings
val getStringArray : string -> TomlDocument -> string list option

/// Get a table (sub-document)
val getTable : string -> TomlDocument -> TomlDocument option
```

## Building from Source

```bash
git clone https://github.com/FidelityFramework/Fidelity.Toml.git
cd Fidelity.Toml
dotnet build
dotnet test
```

## Contributing

Contributions are welcome! Please see the [Fidelity Framework contributing guidelines](https://github.com/FidelityFramework/.github/blob/main/CONTRIBUTING.md).

## License

MIT License - see [LICENSE](LICENSE) for details.

---

*Part of the [Fidelity Framework](https://github.com/FidelityFramework) - Native F# for everyone.*
