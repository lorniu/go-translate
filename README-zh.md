# Go 翻译框架

放假，带娃。忙里偷闲，将这个插件重写了。

支持单引擎、多引擎。目前添加的引擎有:
- Bing 翻译 (最快)
- Google Translate，旧的 API (不挂代理也可访问)
- Google Translate，新的 RPC API
- DeepL，需要自行提供 auth_key

支持多种渲染方式。目前添加的渲染方式有:
- 通过 buffer 显示结果 (默认)
- 将结果拷贝到 Kill-Ring
- 通过 posframe 在当前位置弹出结果
- 通过固定一个 posframe 窗口进行渲染

## 基本使用

添加到配置文件:
```elisp
(require 'go-translate)

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
        (gts-bing-engine)
        ;;(gts-google-engine)
        ;;(gts-google-rpc-engine)
        ;;(gts-deepl-engine :auth-key [YOUR_AUTH_KEY] :pro nil)
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
- 常用参数，可以到 `customize-group` - `go-translate` 进行修改
- 界面的 face，到 `customize-group` - `go-translate-faces` 进行修改

## 定制自己的命令

除了设置 `gts-default-translator` 并使用 `gts-do-translate` 外，你可以灵活定制其他命令。

比如，直接拾取光标位置或选取的内容，使用 Google RPC API 进行翻译:
```elisp
(defun my-translate-command-1 ()
  (interactive)
  (gts-translate (gts-translator
                  :picker (gts-noprompt-picker)
                  :engines (gts-google-rpc-engine)
                  :render (gts-buffer-render))))
```

比如，直接拾取，并将结果添加到 Kill-Ring 中:
```elisp
(defun my-translate-command-2 ()
  (interactive)
  (gts-translate (gts-translator
                  :picker (gts-noprompt-picker)
                  :engines (gts-google-rpc-engine)
                  :render (gts-kill-ring-render))))
```

比如，在光标位置弹出翻译结果:
```elisp
(defun my-translate-command-3 ()
  (interactive)
  (gts-translate (gts-translator
                  :picker (gts-prompt-picker)
                  :engines (gts-google-rpc-engine)
                  :render (gts-posframe-pop-render))))
```

比如，聚合 Google/DeepL 的翻译结果到固定的 Childframe 窗口:
```elisp
(defun my-translate-command-4 ()
  (interactive)
  (gts-translate (gts-translator
                  :picker (gts-prompt-picker)
                  :engines (list (gts-google-rpc-engine :parser (gts-google-rpc-summary-parser)) (gts-deepl-engine))
                  :render (gts-kill-ring-render))))
```

为了避免每次调用命令都创建对象的开销，你可以这样定义你的命令:
```elisp
;; 定义一个变量保存你的 translator，避免每次都重建
(defvar my-translator-n
  (gts-translator :picker .. :engines .. :render ..))

;; 你的命令引用创建好的其他 Component
(defun my-translate-command-n ()
  (interactive)
  (gts-translate my-translator-n)
```

随意组合。

Whatever you like.

## 内置组件简介
### gts-buffer-render

默认使用 `gts-buffer-render` 显示结果。在结果窗口有如下快捷键:
- `h` 显示帮助
- `g` 重新翻译
- `x` 交换 `source language` 和 `target language` 翻译
- `M-n` 和 `M-p`, 切换下一组语言并重新翻译
- `y` 语音播报 (tts)。如果 `gts-tts-try-speak-locally` 为 t，那如果引擎没有提供 tts 服务，将尽量使用本地的 TTS 功能进行播报。比如 windows 上使用 powershell 脚本
- `t` 开关 `gts-buffer-follow-p`。如果设置 `gts-buffer-follow-p` 为 t，翻译完成将会自动跳到 buffer 窗口。通过 `C-x C-x` 可以快速选择翻译结果
- `C` 清空缓存

如果对默认的显示不满意，你可以继承并重写 `gts-buffer-render` 的相关方法。

也可以从头开始实现自己的 parser/render。

### gts-posframe-pop-render/gts-posframe-pin-render

可以安装 `posframe` 后使用 childframe 进行结果显示。有两种风格:
1. gts-posframe-pop-render，在光标位置弹出 childframe，出现结果后，除非立刻切换到 frame，任何操作都导致 frame 消失
2. gts-posframe-pin-render，固定一个不自动消失的 childframe 用来显示结果。这个 frame 窗口是可拖拽的

另外，你可以自由配置 frame 的样式、位置等。在 frame 中可以使用跟 `gts-buffer-render` 中一样的快捷键。
比如 `q` 表示退出。

### gts-prompt-picker/gts-noprompt-picker

用来拾取 text/from/to，通过提供 texter 决定初始化内容是什么:
- gts-prompt-picker，会使用 minibuffer 互动式输入 text/from/to
- gts-noprompt-picker，不进行任何提醒，直接使用合适的 text/from/to 进行翻译

gts-picker 使用 gts-texter 获取初始输入，默认的 texter 会获取当前光标的文本或选取文本。

你也可以定制并使用自己的 texter，比如实现 OCR 输入，语音输入等...

由 gts-prompt-picker 弹出的 minibuffer 中的快捷键:
- `C-l` 清空输入
- `C-n` 和 `C-p` 切换语言
- `Return` 进行翻译

### gts-xxx-engine

一个 engine 最关键的是两个 method:
- `gts-translate` 用于负责抓取内容，交给 parser 格式化之后，由指定 render 渲染
- `gts-tts` 用于语音播报

目前支持的有 Google, Google-RPC, Bing, DeepL。添加其他的不难，只需要了解基本规则即可。

## 实现自己的组件

几乎每一部分都是可以替换的，从 logger/cacher/http-client 到 picker/engine/render 等。

比如，要将翻译结果插入到当前位置，那么实现一个 Render 即可。

代码示例:
```elisp
;; 创建一个类
(defclass your-render (gts-render) ())

;; 重写一个方法
(cl-defmethod gts-out ((_ your-render) task)
  (deactivate-mark)
  (insert (oref task result)))
```

然后，使用就可以了:
```elisp
(defun my-translate-command-5 ()
  (interactive)
  (gts-translate (gts-translator
                  :picker (gts-noprompt-picker)
                  :engines (gts-google-rpc-engine)
                  :render (your-render) ; 使用！
                  )))
```

当然，新建翻译引擎也比较简单:
- 创建一个 `gts-engine` 类，实现其 `gts-translate/gts-tts` 方法，分别用于翻译和语音播报。后面一个可选
- 创建一个 `gts-parser` 类，实现其 `gts-parse` 方法，用于将 engine 传来的字符串，格式化为最终渲染的字符串

再比如，如果你无法忍受 `url.el` 的缓慢，你可以借助 curl/request.el 等实现并替换自己的 gts-http-client 组件。示例:
```elisp
  (require 'request)

  (defclass gts-request-http-client (gts-http-client) ())

  (cl-defmethod gts-request ((o gts-request-http-client) url &key done fail data headers)
    (let ((url-user-agent gts-user-agent))
      (request url
        :data data
        :type (if data "POST" "GET")
        :headers headers
        :parser #'buffer-string
        :success (cl-function (lambda (&key data &allow-other-keys)
                                (with-temp-buffer
                                  (insert data)
                                  (goto-char (point-min))
                                  (funcall done))))
        :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                              (funcall fail error-thrown))))))

  (setq gts-default-http-client (gts-request-http-client))
```

## 设计思路

这个架构，不仅适合翻译，还适合很多类似的场景:
```
初始输入 - 根据输入从网络中获取内容 - 将返回的内容格式化 - 输出到指定位置

picker -> engine -> parser -> render
```

抽象化，组件化。除了核心逻辑，其他都可插拔。

以后有时间再细聊。

## 欢迎提供反馈跟建议

要打开调试，需要将 `gts-debug-p` 设为 t。然后切换到 "` *gts-logger*`" 就能查看到相关日志。

欢迎提供你的一些想法和建议。

更多文档以后补充。
