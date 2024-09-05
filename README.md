# VSCode extension for NMTRAN (NONMEM)

![GitHub](https://img.shields.io/github/license/vrognas/vscode-nmtran)
![Visual Studio Marketplace Installs - Azure DevOps Extension](https://img.shields.io/visual-studio-marketplace/azure-devops/installs/total/vrognas.nmtran)

A [Visual Studio Code](https://code.visualstudio.com/) [extension](https://marketplace.visualstudio.com/VSCode) with support for the NMTRAN language used in [NONMEM](https://www.iconplc.com/solutions/technologies/nonmem/) control streams.

## Requirements

VSCode v1.80.0+

## Installation

To install the extension, open the Extensions view, search for `nonmem` to filter results and select the NMTRAN extension.

## Programmatic Language Features

* Diagnostics
* Code completion proposals
* Hover info

## Declarative Language Features

* Comment toggling using the VS Code command `Toggle Line Comment` 
* Folding (by control records)
* Bracket matching
* Bracket autoclosing
* Bracket autosurrounding

### Syntax Highlighting

By tokenization according to [TextMate 1.5.1 naming conventions](https://macromates.com/manual/en/language_grammars#naming_conventions)

![demo_syntax-highlight](images/demo_syntax-highlight.png)

### Snippet Completion

![demo_advan-snippets](images/demo_advan-snippets.gif)

#### Snippets
* Subroutine selection
  * ADVAN and TRANS
* Modify `$DATA` on-the-fly (Credit: Simon Buatois)
* RUV (normal or log-scale)
  * RUV_add
  * RUV_prop
  * RUV_addprop
* Creating an Xpose-friendly $TABLE scaffold (just type $TABLE).
* MIXTURE-models (just type $MIX)
  * 2-way mixture model
  * 3-way mixture model
* Including IIV on a parameter that is bound between 0 and 1 (type logit_iiv).
* Baseline modeling (B1, B2, B3, B4) [Dansirikul et al., 2008](https://doi.org/10.1007/s10928-008-9088-2)
* BQL modeling (M3) [Beal, 2001](https://doi.org/10.1023/a:1012299115260)

## Contributing

Your contribution to the project is encouraged.
You can report issues and post suggestions of features via [GitHub issues](https://github.com/vrognas/vscode-nmtran/issues).
Thank you!

## License

[MIT](LICENSE)
