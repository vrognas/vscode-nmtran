# NMTRAN (NONMEM) Language Extension for VSCode <img src="images/nmtran.png" align="right" height="160" alt="NMTRAN Logo" />

![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/vrognas/vscode-nmtran/ci.yml)
![Visual Studio Marketplace Version](https://img.shields.io/visual-studio-marketplace/v/vrognas.nmtran)
![Visual Studio Marketplace Installs](https://img.shields.io/visual-studio-marketplace/i/vrognas.nmtran)
![Visual Studio Marketplace Downloads](https://img.shields.io/visual-studio-marketplace/d/vrognas.nmtran)
![GitHub License](https://img.shields.io/github/license/vrognas/vscode-nmtran)

> [!NOTE]
> This extension is currently in active development.
> While feature-complete and stable for daily use, expect occasional breaking changes.
> Please report any issues to help improve the extension.

Comprehensive language support for NMTRAN (NONMEM Translator) files in Visual Studio Code. Provides intelligent features for pharmacometric modeling with NONMEM control streams.

## What is NMTRAN?

[NMTRAN](https://nmhelp.tingjieguo.com) is the control language for [NONMEM](https://www.iconplc.com/solutions/technologies/nonmem/), the gold standard software for population pharmacokinetic/pharmacodynamic (PopPK/PD) modeling in pharmaceutical research.
NMTRAN files (`.mod`, `.ctl`) define models for analyzing drug concentration-time data and dose-response relationships.

## Installation

The NMTRAN extension is available for **VSCode** and **VSCode-based editors** including [Positron](https://github.com/posit-dev/positron), [VSCodium](https://vscodium.com/), and other VSCode clones.

**Install via Extensions View:**
- Open your code editor
- Make sure you have the latest version of the editor (requirement VSCode v1.102.0+)
- Open the Extensions View
- Search for `NMTRAN` or `nonmem`
- Click "Install" on the NMTRAN extension by vrognas

> [!TIP]
> For an enhanced NMTRAN development experience, consider installing:
> **[ErrorLens](https://github.com/usernamehw/vscode-error-lens)**.
> It makes diagnostics stand out more prominently, highlighting the entire line wherever a diagnostic is generated and printing the message inline.
> Perfect for spotting NMTRAN validation errors at a glance.

## Quick Start

Once installed, open any NMTRAN file (`.mod`, `.ctl`) to automatically activate the extension:

Try these features:
- **Hover** over THETA/ETA/EPS to see definitions; control records to see explanations.
- **Right-click** on `THETA(1)` → "Peek" → "Peek References"
- Notice real-time **error highlighting** for invalid syntax

## Features

### 🧠 Language Intelligence

- **Real-time Diagnostics**: Validation of control records, parameter sequences, and NMTRAN syntax
- **Intelligent Hover**: Explanations for control records, parameters, and NONMEM functions
- **Go to Definition**: Navigate from parameter usage (`THETA(1)`) to declaration (`$THETA`)
- **Find All References**: Locate all usages of parameters throughout your model

### ✨ Code Enhancement

- **Syntax Highlighting**: Rich tokenization following TextMate conventions
- **Code Folding**: Collapse control records for better file navigation
- **Smart Formatting**: Configurable indentation and code organization
- **Comment Toggling**: Quick comment/uncomment with standard VSCode commands

![Syntax Highlighting Demo](images/demo_syntax-highlight.png)

### 📝 Code Snippets

Comprehensive snippet library for rapid model development:

![Snippet Demo](images/demo_advan-snippets.gif)

**Available Snippets:**
- **Subroutines**: ADVAN/TRANS combinations for PK modeling
- **Data Handling**: `$DATA` record templates with common options
- **Error Models**: Additive, proportional, and combined residual error models
- **Tables**: Xpose-friendly `$TABLE` configurations
- **Mixture Models**: 2-way and 3-way mixture model templates
- **Special Modeling**:
  - Logit-normal IIV for bounded parameters
  - Baseline models (B1--B4) [Dansirikul et al., 2008](https://doi.org/10.1007/s10928-008-9088-2)
  - BQL handling (M3 method) [Beal, 2001](https://doi.org/10.1023/a:1012299115260)

## Supported File Types
https://github.com/vrognas/vscode-nmtran/blob/main/README.md
The extension activates for these NONMEM-related file extensions:

- **Control Streams**: `.mod`, `.ctl`, `.modt`
- **Output Files**: `.lst`, `.ext`, `.cov`, `.cor`, `.coi`, `.cnv`
- **Special Files**: `.phi`, `.scm`, `.grd`, `.shk`, `.shm`, `.smt`, `.rmt`, `.phm`

## Community & Support

- **Documentation**: [NMTRAN Reference](https://nmhelp.tingjieguo.com)
- **Issues & Feature Requests**: [GitHub Issues](https://github.com/vrognas/vscode-nmtran/issues)
- **Discussions**: [GitHub Discussions](https://github.com/vrognas/vscode-nmtran/discussions)
- **Contributing**: See [CONTRIBUTING.md](CONTRIBUTING.md)
- **Sponsor**: [Buy me a coffee ☕](https://buymeacoffee.com/vrognas)

## Contributing

Whether you're fixing bugs, adding features, or improving documentation, your help makes this extension better for the pharmacometrics community.

- 🐛 **Bug Reports**: [GitHub Issues](https://github.com/vrognas/vscode-nmtran/issues)
- 💡 **Feature Requests**: [GitHub Discussions](https://github.com/vrognas/vscode-nmtran/discussions)
- 🔧 **Pull Requests**: See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines
- 📖 **Documentation**: Help improve our docs and examples

## License

[MIT License](LICENSE) - feel free to use this extension in your research and commercial projects.

---

**Enjoy enhanced NMTRAN development!** 🧬💊

*Made with ❤️ for the pharmacometrics community*
