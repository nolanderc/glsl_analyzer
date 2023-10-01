
# `glsl_analyzer`

Language server for GLSL (OpenGL Shading Language).

- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
    - [Neovim](#neovim)


## Features

- Completion 
    - User-defined variables/functions.
    - Built-in types (e.g., `vecN`, `matNxM`, `texture2D`, etc.)
    - Built-in functions (e.g., `length`, `imageLoad`, `packUnorm4x8`)
    - Includes all [extensions](https://github.com/KhronosGroup/GLSL#extension-specifications-in-this-repository)
- Goto Definition
- Inline hover documentation for all builtin and extension functions/variables
- Support for `#include`
- Formatter

![](resources/screenshot.png)


### In the pipeline

- Completion of fields
- Support for refactors (renaming)


## Installation

```sh
zig build install -Doptimize=ReleaseSafe --prefix ~/.local/
```

Tested using `zig 0.12.0-dev.629+1606717b5`.


## Usage

By default `glsl_analyzer` communicates over stdin/stdout:

```sh
glsl_analyzer
```

However, you can configure it to use a specific port (TCP) using the following command:

```sh
glsl_analyzer --port <PORT>
```

### Neovim

[`nvim-lspconfig`](https://github.com/neovim/nvim-lspconfig) comes with support for `glsl_analyzer`. Simply add the following to your lua config:

```lua
require'lspconfig'.glsl_analyzer.setup{}
```
