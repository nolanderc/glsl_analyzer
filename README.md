
# `glsl_analyzer`

Language server for GLSL (OpenGL Shading Language).

- [Features](#features)
- [Installation](#installation)
    - [Building from Source](#building-from-source)
- [Usage](#usage)
    - [Neovim](#neovim)
    - [Visual Studio Code](#visual-studio-code)


## Features

- Completion 
    - User-defined variables/functions.
    - Built-in types (e.g., `vecN`, `matNxM`, `texture2D`, etc.)
    - Built-in functions (e.g., `length`, `imageLoad`, `packUnorm4x8`)
    - Includes all [extensions](https://github.com/KhronosGroup/GLSL#extension-specifications-in-this-repository)
    - Fields
- Goto Definition
- Inline hover documentation for all builtin and extension functions/variables
- Support for `#include`
- Formatter


### Screenshots

#### Completion
![completion](https://github.com/nolanderc/glsl_analyzer-vscode/blob/main/screenshots/completion.gif)

#### Formatting
![formatting](https://github.com/nolanderc/glsl_analyzer-vscode/blob/main/screenshots/formatting.gif)

#### Goto-Definition
![goto definition](https://github.com/nolanderc/glsl_analyzer-vscode/blob/main/screenshots/goto-definition.gif)


### In the pipeline

- Support for refactors (renaming)


## Installation

We provide precompiled binaries for Linux, MacOS and Windows on the
[Releases](https://github.com/nolanderc/glsl_analyzer/releases) page.
Make sure to copy it somewhere under your `PATH` environment variable (e.g.
`~/.local/bin` on Linux).


### Building from Source


```sh
zig build install -Doptimize=ReleaseSafe --prefix ~/.local/
```

Tested using `zig 0.14.0`.


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

### Visual Studio Code

Install the
[`glsl-analyzer`](https://marketplace.visualstudio.com/items?itemName=nolanderc.glsl-analyzer)
extension from the marketplace. It will automatically download the latest precompiled binary for your platform.
