# Go 翻译框架

放假，带娃。忙里偷闲，将这个插件重写了。

支持单引擎、多引擎。目前添加的引擎有:
- Bing 翻译 (最快)
- Google Translate，旧的 API (不挂代理也可访问)
- Google Translate，新的 RPC API
- DeepL

支持多种渲染方式。目前添加的渲染方式有:
- 将结果拷贝到 Kill-Ring
- 通过 buffer 显示结果
- 通过 posframe 在当前位置弹出结果
- 通过固定一个 posframe 窗口进行渲染 (推荐)

## 基本使用

添加到配置文件:
```elisp
(require 'gts)

(setq gts-translate-list '(("en" "zh")))
(setq gts-default-translator
      (gts-translator
       :picker (gts-prompt-picker)
       :engines (list (gts-google-engine) (gts-google-rpc-engine))
       :render (gts-buffer-render)))
```

然后使用 `gts-do-translate` 命令进行翻译。

## 更多配置

```elisp
;; 配置多个翻译语言对
(setq gts-translate-list '(("en" "zh") ("fr" "zh")))

;; 配置默认的 translator
;; 这些配置将被 gts-do-translate 命令使用
(setq gts-default-translator
      (gts-translator

       :picker ; 用于拾取初始文本、from、to，只能配置一个

       ;;(gts-noprompt-picker)
       ;;(gts-noprompt-picker :texter (gts-whole-buffer-texter))
       (gts-prompt-picker)
       ;;(gts-prompt-picker :single t)
       ;;(gts-prompt-picker :texter (gts-current-or-selection-texter) :single t)

       :engines ; 翻译引擎，可以配置多个。另外可以传入不同的 Parser 从而使用不同样式的输出

       (list
        (gts-bing-cn-engine)
        ;;(gts-google-engine)
        ;;(gts-google-rpc-engine)
        ;;(gts-deepl-engine :auth-key "3e10bade-88e9-02f2-269f-ab3c445d7984:fx" :pro nil)
        (gts-google-engine :parser (gts-google-summary-parser))
        ;;(gts-google-engine :parser (gts-google-parser))
        ;;(gts-google-rpc-engine :parser (gts-google-rpc-summary-parser))
        (gts-google-rpc-engine :parser (gts-google-rpc-parser))
        )

       :render ; 渲染器，只能一个，用于输出结果到指定目标。如果使用 childframe 版本的，需自行安装 posframe

       (gts-buffer-render)
       ;;(gts-posframe-pop-render)
       ;;(gts-posframe-pop-render :backcolor "#333333" :forecolor "#ffffff")
       ;;(gts-posframe-pin-render)
       ;;(gts-posframe-pin-render :position (cons 1200 20))
       ;;(gts-posframe-pin-render :width 80 :height 25 :position (cons 1000 20) :forecolor "#ffffff" :backcolor "#111111")
       ;;(gts-kill-ring-render)
       ))
```

其他:
- 常用参数，可以到 `customize-group` - `gts` 进行修改
- 界面的 face，到 `customize-group` - `gts-faces` 进行修改

## 定制自己的命令

比如，直接拾取光标位置或选取的内容，使用 Google RPC API 进行翻译:
```elisp
(defun my-translate-command-1 ()
  (interactive)
  (do-translate (gts-translator
				 :picker (gts-noprompt-picker)
				 :engines (gts-google-rpc-engine)
				 :render (gts-buffer-render))))
```

比如，直接拾取，并将结果添加到 Kill-Ring 中:
```elisp
(defun my-translate-command-2 ()
  (interactive)
  (do-translate (gts-translator
				 :picker (gts-noprompt-picker)
				 :engines (gts-google-rpc-engine)
				 :render (gts-kill-ring-render))))
```

比如，在光标位置弹出翻译结果:
```elisp
(defun my-translate-command-3 ()
  (interactive)
  (do-translate (gts-translator
				 :picker (gts-prompt-picker)
				 :engines (gts-google-rpc-engine)
				 :render (gts-posframe-pop-render))))
```

比如，聚合 Google/DeepL 的翻译结果到固定的 Childframe 窗口:
```elisp
(defun my-translate-command-4 ()
  (interactive)
  (do-translate (gts-translator
				 :picker (gts-prompt-picker)
				 :engines (list (gts-google-rpc-engine :parser (gts-google-rpc-summary-parser)) (gts-deepl-engine))
				 :render (gts-kill-ring-render))))
```

随意组合。

Whatever you like.

## 实现自己的组件

每一部分都是可以替换的。

比如，要将翻译结果插入到当前位置。那么实现一个 Render 即可。

代码示例:
```elisp
;; 创建一个类
(defclass your-render (gts-render) ())

;; 重写一个方法
(cl-defmethod gts-out ((_ your-render) result)
  (deactivate-mark)
  (insert result))
```

然后，使用就可以了:
```elisp
(defun my-translate-command-5 ()
  (interactive)
  (do-translate (gts-translator
				 :picker (gts-noprompt-picker)
				 :engines (gts-google-rpc-engine)
				 :render (your-render) ; 使用！
                 )))
```

当然，新建翻译引擎也比较简单:
- 创建一个 `gts-engine` 类，实现其 `gts-translate/gts-tts` 方法，分别用于翻译和语音播报。后面一个可选
- 创建一个 `gts-parser` 类，实现其 `gts-parse` 方法，用于将 engine 传来的字符串，格式化为最终渲染的字符串

## Miscellaneous

更多文档以后补充。
