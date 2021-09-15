
# Table of Contents

1.  [Emacs Configuration](#org11aa6da)
    1.  [Heavy-handed additions](#org9da35e1)
    2.  [Minor additions](#orgf752277)
    3.  [Future plans](#org35816c9)


<a id="org11aa6da"></a>

# Emacs Configuration

A relatively minimal Emacs configuration.


<a id="org9da35e1"></a>

## Heavy-handed additions

-   The [`ivy/counsel/swiper`](https://github.com/abo-abo/swiper) narrowing suite, using [`prescient`](https://github.com/raxod502/prescient.el) as a backend for sorting matches;

-   Autocompletion with [`company`](https://company-mode.github.io/);

-   Support for Microsoft's Language Server Protocol, and its Emacs interface [`lsp-mode`](https://emacs-lsp.github.io/lsp-mode/)); as backends I configured support for [`ccls`](https://github.com/MaskRay/emacs-ccls) for C/C++ and the Jedi Language Server ([`lsp-jedi`](https://emacs-lsp.github.io/lsp-mode/page/lsp-jedi/)) for Python;

-   Emacs Speaks Statistics ([ESS](https://ess.r-project.org/)), the R-Studio (almost) replacement;


<a id="orgf752277"></a>

## Minor additions

These include [`dashboard`](https://github.com/emacs-dashboard/emacs-dashboard) as a centralised front page, [`which-key`](https://github.com/justbur/emacs-which-key) for real-time keybinding suggestions, and the [`avy`](https://github.com/abo-abo/avy) visual text-jumping package.


<a id="org35816c9"></a>

## Future plans

I will probably include LaTeX support in the near future.

