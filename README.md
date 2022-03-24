
# Table of Contents

1.  [Emacs Configuration](#org1fa754b)
    1.  [init.el - the user configuration](#org0d503d5)
        1.  [Heavy-handed additions](#org916acdc)
        2.  [Minor additions](#org2c6b3bd)
    2.  [root-init.el - the root-user terminal configuration](#org8384941)
    3.  [Future plans](#org2729380)


<a id="org1fa754b"></a>

# Emacs Configuration

A relatively minimal Emacs configuration.


<a id="org0d503d5"></a>

## init.el - the user configuration


<a id="org916acdc"></a>

### Heavy-handed additions

-   The [ivy/counsel/swiper](https://github.com/abo-abo/swiper) narrowing suite, using [prescient](https://github.com/raxod502/prescient.el) as a backend to sort matches;

-   Autocompletion with [company](https://company-mode.github.io/);

-   Support for Microsoft's Language Server Protocol, and the [eglot](https://github.com/joaotavora/eglot) Emacs interface. Configured backends: [ccls](https://github.com/MaskRay/emacs-ccls) for C/C++ and the [Python LSP Server](https://github.com/python-lsp/python-lsp-server) for Python;

-   Emacs Speaks Statistics ([ESS](https://ess.r-project.org/)), the R-Studio quasi-replacement.

-   Support for Python virtual environments, with [pyvenv.el](https://github.com/jorgenschaefer/pyvenv).

-   Support for [GNU Guile](https://www.gnu.org/software/guile/) through [Geiser](https://www.nongnu.org/geiser/).

-   Support for [Common Lisp](https://lisp-lang.org/) through [SLIME](https://github.com/slime/slime).


<a id="org2c6b3bd"></a>

### Minor additions

-   [dashboard](https://github.com/emacs-dashboard/emacs-dashboard) as a quick-access front page;

-   [which-key](https://github.com/justbur/emacs-which-key) for real-time keybinding suggestions;

-   the [avy](https://github.com/abo-abo/avy) visual text-jumping package.


<a id="org8384941"></a>

## root-init.el - the root-user terminal configuration

This init file provides a lightweight terminal-oriented configuration. The use case I had in mind was a customization suitable for system administrators.
As such, it is self-contained: it only relies on the packages already included with the GNU Emacs installation.
It should be copied or linked as `/root/.emacs.d/init.el`.


<a id="org2729380"></a>

## Future plans

I will probably include LaTeX IDE features (and `pdf-tools`) in the near future.

