
# Table of Contents

1.  [Emacs Configuration](#org1cf096f)
    1.  [init.el - the user configuration](#org3546a99)
        1.  [Heavy-handed additions](#org133ff77)
        2.  [Minor additions](#orge7e2561)
    2.  [root-init.el - the root-user terminal configuration](#org441934b)
    3.  [Future plans](#org5205799)


<a id="org1cf096f"></a>

# Emacs Configuration

A relatively minimal Emacs configuration.


<a id="org3546a99"></a>

## init.el - the user configuration


<a id="org133ff77"></a>

### Heavy-handed additions

-   The [ivy/counsel/swiper](https://github.com/abo-abo/swiper) narrowing suite, using [prescient](https://github.com/raxod502/prescient.el) as a backend to sort matches;

-   Autocompletion with [company](https://company-mode.github.io/);

-   Support for Microsoft's Language Server Protocol, and the [eglot](https://github.com/joaotavora/eglot) Emacs interface. Configured backends: [ccls](https://github.com/MaskRay/emacs-ccls) for C/C++ and the [Python LSP Server](https://github.com/python-lsp/python-lsp-server) for Python;

-   Emacs Speaks Statistics ([ESS](https://ess.r-project.org/)), the R-Studio quasi-replacement.

-   Support for Python virtual environments, with [pyvenv.el](https://github.com/jorgenschaefer/pyvenv).

-   Support for [GNU Guile](https://www.gnu.org/software/guile/) through [Geiser](https://www.nongnu.org/geiser/).


<a id="orge7e2561"></a>

### Minor additions

-   [dashboard](https://github.com/emacs-dashboard/emacs-dashboard) as a quick-access front page;

-   [which-key](https://github.com/justbur/emacs-which-key) for real-time keybinding suggestions;

-   the [avy](https://github.com/abo-abo/avy) visual text-jumping package.


<a id="org441934b"></a>

## root-init.el - the root-user terminal configuration

This init file provides a lightweight terminal-oriented configuration. The use case I had in mind was a customization suitable for system administrators.
As such, it is self-contained: it only relies on the packages already included with the GNU Emacs installation.
It should be copied or linked as `/root/.emacs.d/init.el`.


<a id="org5205799"></a>

## Future plans

I will probably include LaTeX IDE features (and `pdf-tools`) in the near future.

