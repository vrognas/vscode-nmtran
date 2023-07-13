# VSCode extension for NMTRAN (NONMEM) ![GitHub](https://img.shields.io/github/license/vrognas/vscode-nmtran) ![Visual Studio Marketplace Installs - Azure DevOps Extension](https://img.shields.io/visual-studio-marketplace/azure-devops/installs/total/vrognas.nmtran)

A [Visual Studio Code](https://code.visualstudio.com/) [extension](https://marketplace.visualstudio.com/VSCode) with support for the NMTRAN language used in [NONMEM](https://www.iconplc.com/solutions/technologies/nonmem/) control streams.

## Installation

To install the extension, open the Extensions view, search for `nonmem` to filter results and select NMTRAN extension.

## Overview of the extension features

### Syntax highlighting

![demo_syntax-highlight](images/demo_syntax-highlight.png)

### Snippets

![demo_advan-snippets](images/demo_advan-snippets.gif)

Snippets for RUV (normal or log-scale):
* RUV_add
* RUV_prop
* RUV_addprop

Snippets for MIXTURE-models (just type $MIX):
* 2-way mixture model
* 3-way mixture model

Snippets for creating an Xpose-friendly $TABLE scaffold (just type $TABLE).

Snippet for including IIV on a parameter that is bound between 0 and 1 (type logit_iiv).

### Declarative Language Features

* Comment toggling using the VS Code command `Toggle Line Comment` 
* Autoclosing of brackets

## Requirements

VSCode v1.80.0

## Contributing

We encourage your contribution to the project. You can report issues and post suggestions of features via [GitHub issues](https://github.com/vrognas/vscode-nmtran/issues). Thank you!

## License

[MIT](LICENSE)