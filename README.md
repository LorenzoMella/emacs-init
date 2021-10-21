
# Table of Contents

1.  [Emacs Configuration](#org0cf5544)
    1.  [init.el - the user configuration](#org4302928)
        1.  [Heavy-handed additions](#orgaf7eb62)
        2.  [Minor additions](#org9012e3c)
    2.  [root-init.el - the root-user terminal configuration](#org4b9bc95)
    3.  [Future plans](#orgeddec46)


<a id="org0cf5544"></a>

# Emacs Configuration

A relatively minimal Emacs configuration.


<a id="org4302928"></a>

## init.el - the user configuration


<a id="orgaf7eb62"></a>

### Heavy-handed additions

-   The [ivy/counsel/swiper](https://github.com/abo-abo/swiper) narrowing suite, using [prescient](https://github.com/raxod502/prescient.el) as a backend to sort matches;

-   Autocompletion with [company](https://company-mode.github.io/);

-   Support for Microsoft's Language Server Protocol, and its Emacs interface [lsp-mode](https://emacs-lsp.github.io/lsp-mode/). Configured backends: [ccls](https://github.com/MaskRay/emacs-ccls) for C/C++ and the Jedi Language Server ([lsp-jedi](https://emacs-lsp.github.io/lsp-mode/page/lsp-jedi/)) for Python;

-   Emacs Speaks Statistics ([ESS](https://ess.r-project.org/)), the R-Studio quasi-replacement.


<a id="org9012e3c"></a>

### Minor additions

-   [dashboard](https://github.com/emacs-dashboard/emacs-dashboard) as a quick-access front page;

-   [which-key](https://github.com/justbur/emacs-which-key) for real-time keybinding suggestions;

-   the [avy](https://github.com/abo-abo/avy) visual text-jumping package.


<a id="org4b9bc95"></a>

## root-init.el - the root-user terminal configuration

This init file is a simple lightweight terminal configuration. The use case I had in mind was a customization suitable for the root user.
As such, it is self-contained: it only relies on the packages already included with the GNU Emacs installation.
It should be copied or linked as `/root/.emacs.d/init.el`.


<a id="orgeddec46"></a>

## Future plans

I will probably include LaTeX IDE features (and `pdf-tools`) in the near future.

