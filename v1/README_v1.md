[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![MELPA](https://melpa.org/packages/go-translate-badge.svg)](https://melpa.org/#/go-translate)

# Go-Translate (v1, deprecated)

Improved Google Translate interface with asynchronous request and better user experience.

[点击查看中文版文档](README-zh_v1.md)

I have used translation frequently in recent days.

I tried [Google Translate](https://github.com/atykhonov/google-translate), it is cool, but something troubled me:
- It's synchronous, every query will block anything, and the user experience is not good enough, especially when the network is poor
- The result buffer is not formatted enough, and the operation is less user-friendly
- The code is too long too old, make it hard to hack and extend new functions

So I wrote this plugin. It borrows some ideas and codes from [Google Translate](https://github.com/atykhonov/google-translate), and its main features are:
- Completely asynchronous, without any blocking problems, so very smoothly
- Optimize the display of translation results, the interface is more beautiful and natural
- It is convenient for switch between different languages, and support many interactive operations
- The code is clean, and easy to expand. New functions can be constructed very simply

## Installation

With MELPA and package.el:
```
M-x package-install RET go-translate RET

(require 'go-translate)
```

Or manually download `go-translate.el` then put into `/some-path`, then add this to `.emacs`:
```elisp
(add-to-list 'load-path "/some-path")
(require 'go-translate)
```

## Configuration

First, config the local language and your default target language:
```elisp
(setq go-translate-local-language "en")
(setq go-translate-target-language "fr")
```

If you have any other translation plans, then put them into `go-translate-extra-directions`:
```elisp
(add-to-list go-translate-extra-directions (cons "ja" "ru"))  ; from japanese to russian
(add-to-list go-translate-extra-directions (cons "en" "fr"))  ; from english to french

;; or
(setq go-translate-extra-directions '(("ja" . "ru") ("en" . "fr")))
```

Other customizations you can look into `custom-group` - `go-translate`, eg:
```elisp
(setq go-translate-buffer-follow-p t)       ; focus the result window
(setq go-translate-buffer-source-fold-p t)  ; fold the source text in the result window
(setq go-translate-buffer-window-config ..) ; config the result window as your wish
```

## Usage

The core command is `go-translate`, and there are several extended commands such as `go-translate-popup`.
You can bind keys for them. such as:
```elisp
(global-set-key "\C-ct" 'go-translate)
(global-set-key "\C-cT" 'go-translate-popup)
```

`go-translate` will use a buffer to display translation results. In the result buffer, there are several shortcut keys:
- `g` refresh `q` exit
- `x` exchanges `source language` and `target language` and refresh the translation
- `M-n` and `M-p`, switch to the next/prev available translation direction, and refresh
- `y` to speak the current selection or word. You should have `mplayer` installed, or on Windows it will fallback to use `powershell` to do the tts job.

The `go-translate` will take the currently selected text or `word-at-point` as the default input.
In the pop-up `read-from-minibuffer` interface, you can use `C-l` to clear the input, and fire the translation
with `Return` or `C-return`. `C-return` will force the cursor follow the result window after translation.
Also, you can use `C-n` and `C-p` to switch translation direcitons. These direcitons are those configured in `go-translate-extra-directions` above.

Other commands based on `go-translate`:
- `go-translate-popup` is to display a short translation by popping up a `posframe` at the cursor.
- `go-translate-popup-current` is to pop the result for the current selection or word, without prompt.
- `go-translate-kill-ring-save` will not pop any user interface, but save the result into `king-ring` for later use.


Extending commands is easy and happy, go and have a try!

## Extend

If you want to expand your own command, you only need to overwrite or let-binding the variables below:
- `go-translate-text-function` the default translation content. If not specified, the text selected or at the cursor will be read
- `go-translate-inputs-function` is used to process user input and select translation languages
- `go-translate-url-function` is used to generate the request url
- `go-translate-prepare-function` some preparatory work done before the request is send. For example, create a buffer and render something
- `go-translate-request-function` asynchronously request to the server to get the translation content
- `go-translate-render-function` render the returned result

You can take the source code of `go-translate-popup` for example.

In addition, a number of `go-translate-result-*` methods are also encapsulated to extract relevant content from the request result.
They are useful when customizing the render function.

## Miscellaneous

Nothing more to say now.
