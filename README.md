[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![MELPA](https://melpa.org/packages/go-translate-badge.svg)](https://melpa.org/#/go-translate)

# Go-Translate

I rewrote this plugin to make it more flexible and powerful. May it will be the best translate plugin on Emacs.

- [中文版文档 - 重构了，不止支持 Google，不止是一个翻译框架](README-zh.md)
- [Documentation of the old version (v1, deprecated)](v1/README_v1.md)

In addition to Google Translate, it now supports more engines like Google RPC API, Bing, DeepL.
You can easily add other translation engines on the basis of the framework.

Very scalable, very flexible, asynchronous request and better user experience.

The reason I rewrite this is that I want to add the new RPC API of Google translation to this plugin.
Since adding new API, it's not nice to implement the plugin as a translation framework first.

Then, there is it.

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

## Basic Usage

Add this to your configuration file:
```elisp
(require 'go-translate)

(setq gts-translate-list '(("en" "zh")))

(setq gts-default-translator
      (gts-translator
       :picker (gts-prompt-picker)
       :engines (list (gts-bing-engine) (gts-google-engine))
       :render (gts-buffer-render)))
```

Then use `gts-do-translate` to start translation.

## More configuration

```elisp
;; your languages pair used to translate
(setq gts-translate-list '(("en" "zh") ("fr" "en")))

;; config the default translator, it will be used by command gts-do-translate
(setq gts-default-translator
      (gts-translator

       :picker ; used to pick source text, from, to. choose one.

       ;;(gts-noprompt-picker)
       ;;(gts-noprompt-picker :texter (gts-whole-buffer-texter))
       (gts-prompt-picker)
       ;;(gts-prompt-picker :single t)
       ;;(gts-prompt-picker :texter (gts-current-or-selection-texter) :single t)

       :engines ; engines, one or more. Provide a parser to give different output.

       (list
        (gts-bing-engine)
        ;;(gts-google-engine)
        ;;(gts-google-rpc-engine)
        ;;(gts-deepl-engine :auth-key [YOUR_AUTH_KEY] :pro nil)
        (gts-google-engine :parser (gts-google-summary-parser))
        ;;(gts-google-engine :parser (gts-google-parser))
        ;;(gts-google-rpc-engine :parser (gts-google-rpc-summary-parser) :url "https://translate.google.com")
        (gts-google-rpc-engine :parser (gts-google-rpc-parser) :url "https://translate.google.com")
        )

       :render ; render, only one, used to consumer the output result. Install posframe yourself when use gts-posframe-xxx

       (gts-buffer-render)
       ;;(gts-posframe-pop-render)
       ;;(gts-posframe-pop-render :backcolor "#333333" :forecolor "#ffffff")
       ;;(gts-posframe-pin-render)
       ;;(gts-posframe-pin-render :position (cons 1200 20))
       ;;(gts-posframe-pin-render :width 80 :height 25 :position (cons 1000 20) :forecolor "#ffffff" :backcolor "#111111")
       ;;(gts-kill-ring-render)
       ))
```

You can look into `customize-group` - `go-translate` for more configurations.

## Extend your commands

In addition to setup `gts-default-translator` and direct use `gts-do-translate`, you can define your own commands.

Eg, pick directly and use Google RPC API to translate:
```elisp
(defun my-translate-command-1 ()
  (interactive)
  (gts-translate (gts-translator
                  :picker (gts-noprompt-picker)
                  :engines (gts-google-rpc-engine)
                  :render (gts-buffer-render))))
```

Eg, pick directly and add the results into kill-ring:
```elisp
(defun my-translate-command-2 ()
  (interactive)
  (gts-translate (gts-translator
                  :picker (gts-noprompt-picker)
                  :engines (gts-google-rpc-engine)
                  :render (gts-kill-ring-render))))
```

Eg, pop a childframe to show the translation result:
```elisp
(defun my-translate-command-3 ()
  (interactive)
  (gts-translate (gts-translator
                  :picker (gts-prompt-picker)
                  :engines (gts-google-rpc-engine)
                  :render (gts-posframe-pop-render))))
```

Eg, show multiple engines's result (Google/DeepL) in a pin childframe:
```elisp
(defun my-translate-command-4 ()
  (interactive)
  (gts-translate (gts-translator
                  :picker (gts-prompt-picker)
                  :engines (list (gts-google-rpc-engine :parser (gts-google-rpc-summary-parser)) (gts-deepl-engine))
                  :render (gts-kill-ring-render))))
```

To avoid the cost of creating objects every time you call a command, you can define your command this way:
```elisp
;; predefine
(defvar my-translator-n
  (gts-translator :picker .. :engines .. :render ..))

;; reference
(defun my-translate-command-n ()
  (interactive)
  (gts-translate my-translator-n)
```

Compose yourself. Whatever you like.

## Builtin Components

The command `gts-do-translate` will take `gts-default-translator` as the default translator.

### gts-buffer-render

By default, it use `gts-buffer-render` to display translation results. In the result buffer, there are several shortcut keys:
- `h` show help
- `g` refresh `q` exit
- `x` exchanges `source language` and `target language` and refresh the translation
- `M-n` and `M-p`, switch to the next/prev available translation direction, and refresh
- `y` to speak the current selection or word. You should have `mplayer/mpv` installed, or on Windows it will fallback to use `powershell` to do the tts job.
- `C` clear all caches in gts-default-cacher

### gts-posframe-pop-render/gts-posframe-pin-render

Use childframe to show the results.

You should install `posframe` from MELPA first if you want to use these renders.

`gts-posframe-pop-render` will pop a childframe in current position to show the results.
The frame will be disappeared by any user action, except when you focus into it. When you focus into the
frame, you can use all keybindings that like in `gts-buffer-render`.

`gts-posframe-pin-render` will pin a childframe to service for the translation result.
It is draggable and resizable, and you can change the position/color/etc as you wish.

### gts-prompt-picker/gts-noprompt-picker

By default, translator uses `gts-prompt-picker` to pick the source text and translation from/to languages.

`gts-prompt-picker` uses a texter to decide the initial source text, the default texter is `gts-current-or-selection-texter`,
it takes the currently selected text or `word-at-point` as the default source text.

In the pop-up `read-from-minibuffer` interface triggled by `gts-prompt-picker`, you can use `C-l` to clear the input, and fire the translation
with `Return`. Also, you can use `C-n` and `C-p` to switch translation directions. These directions are those configured in `gts-translate-list` above.

The `gts-noprompt-picker` is another choice if you don't like the prompting style's picking.
It will automately take the text from texter and choose a suitable from/to, then translate directly.

## Extend your components

Extending components is easy and happy, go and have a try!

You can replace almost everything. logger/cacher/http-client, picker/texter/render and engine...

For example, you want to insert the results directly into the buffer,
create your own render like below.

Define class and override methods:
```elisp
;; A class
(defclass your-render (gts-render) ())

;; A method
(cl-defmethod gts-out ((_ your-render) task)
  (deactivate-mark)
  (insert (oref task result)))
```

Use them in your translator:
```elisp
(defun my-translate-command-5 ()
  (interactive)
  (gts-translate (gts-translator
                  :picker (gts-noprompt-picker)
                  :engines (gts-google-rpc-engine)
                  :render (your-render) ; yeap!
                  )))
```

Of course, it's relatively easy to build a new translation engine too:
- create a class from `gts-engine`, implement `gts-translate/gts-tts`
- create a class from `gts-parser`, implement `gts-parse`

For example, if you can't stand the poor performance of `url.el`,
you can implement your own `gts-http-client` with curl, etc.

## Miscellaneous

More documentation will be added later.
