;;; init.el --- Emacs initialization file  -*- lexical-binding: t -*-
;;; Commentary:
;; Dependencies
;;    cask
;;    marked (npmで入れる)
;;    pry
;;    pry-doc
;;    method_source
;;    rubocop
;;    ruby-lint
;;    cmigemo (日本語検索)
;;    aspell
;;    ※ migemo のインストール時に文字コードエラーが出るので euc-jp-unix を選んでやること
;;    wakatime (pyenv + pipで入れる)
;; 
;; メモ
;; 現在有効なキーボードショートカットを表示するには<F1> b
;;
;; .emacs.el を再読み込みするには
;; C-x C-s または Command + s
;; M-x load-file RET ~/.emacs.el RET
;;

;; custom-set-variables は別ファイルにする
(setq custom-file (locate-user-emacs-file "custom.el"))
; custom-set-variables 変数更新のためにinit.el が上書きされるのを防ぐ
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;(setq debug-on-message t)

;; initialize straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(if (file-exists-p "~/.emacs.d/.env.el")
    (load "~/.emacs.d/.env.el"))

(defun set-font-size (height)
  (interactive "nHeight:")
  (set-face-attribute 'default (selected-frame) :height height))

;; GUIの設定が後から動くとなんかうざい感じになるので先に動かす
(if (eq window-system 'w32)
    (progn
      (custom-set-faces
       '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "outline" :family "Osaka－等幅")))))
      (set-frame-parameter nil 'alpha 90)
      ;; デフォルトの文字コードはUTF-8にする
      (set-default-coding-systems 'utf-8)
      (prefer-coding-system 'utf-8-unix)

      (setq grep-command "grep -n -e ")
      (setq grep-program "grep")
      (defun open-filer()
	"Open current directory by filer"
	(interactive)
	(shell-command "explorer /e,."))
      (setq default-input-method "W32-IME")
      (setq-default w32-ime-mode-line-state-indicator "[--]")
      (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
      (w32-ime-initialize)
      ;; 日本語入力時にカーソルの色を変える設定 (色は適宜変えてください)
      (add-hook 'w32-ime-on-hook '(lambda () (set-cursor-color "coral4")))
      (add-hook 'w32-ime-off-hook '(lambda () (set-cursor-color "white")))

      ;; 以下はお好みで設定してください
      ;; 全てバッファ内で日本語入力中に特定のコマンドを実行した際の日本語入力無効化処理です
      ;; もっと良い設定方法がありましたら issue などあげてもらえると助かります

      ;; ミニバッファに移動した際は最初に日本語入力が無効な状態にする
      (add-hook 'minibuffer-setup-hook 'deactivate-input-method)

      ;; isearch に移行した際に日本語入力を無効にする
      (add-hook 'isearch-mode-hook '(lambda ()
				      (deactivate-input-method)
				      (setq w32-ime-composition-window (minibuffer-window))))
      (add-hook 'isearch-mode-end-hook '(lambda () (setq w32-ime-composition-window nil)))
      ))

;; ns
(if (eq window-system 'ns)
    (progn
      (load-theme 'wombat t)
      (set-frame-parameter (selected-frame) 'alpha '(75 . 65))
      (tool-bar-mode -1)
      (setq ns-pop-up-frames nil) ;; コマンドから open -a Emacs.app されたときに新しいフレームを開かない
      ;; keybind
      (global-unset-key "\C-z") ;; C-zで最小化してうざいので無効に
      (setq mac-option-modifier 'meta)   ;; Option キーを Meta キーとして使う
      (setq mac-command-modifier 'super) ;; Command キーを Super キーとして使う
      ;; Mac 標準キーバインド
      (global-set-key [(super v)] 'yank)
      (global-set-key [(super c)] 'kill-ring-save)
      (global-set-key [(super s)] 'save-buffer)
      (global-set-key [(super x)] 'kill-region)
      ;; バックスラッシュ入力
      (define-key global-map [2213] nil)
      (define-key global-map [67111077] nil)
      (define-key global-map [134219941] nil)
      (define-key global-map [201328805] nil)
      (define-key function-key-map [2213] [?\\])
      (define-key function-key-map [67111077] [?\C-\\])
      (define-key function-key-map [134219941] [?\M-\\])
      (define-key function-key-map [201328805] [?\C-\M-\\])
      (define-key global-map [3420] nil)
      (define-key global-map [67112284] nil)
      (define-key global-map [134221148] nil)
      (define-key global-map [201330012] nil)
      (define-key function-key-map [3420] [?\\])
      (define-key function-key-map [67112284] [?\C-\\])
      (define-key function-key-map [134221148] [?\M-\\])
      (define-key function-key-map [201330012] [?\C-\M-\\])

      ;; カーソル行ハイライト
      (defface hlline-face
      	'((((class color)
      	    (background dark))
      	   ;;(:background "dark state gray"))
      	   (:background "gray10"
      			:underline "gray4"))
      	  (((class color)
      	    (background light))
      	   (:background "ForestGreen"
      			:underline nil))
      	  (t ()))
      	"*Face used by hl-line.")
      (setq hl-line-face 'hlline-face)
      ;; (setq hl-line-face 'underline)
      (global-hl-line-mode)

      ;; Show filename on titlebar
      (setq frame-title-format (format "%%f - Emacs"))

      ;; disable x-popup-dialog
      (defun my-prevent-dialog-advice (orig-fun &rest args)
        "Prevent dialog activation by temporarily disabling use-dialog-box"
        (let ((use-dialog-box nil))
          (apply orig-fun args)))
      
      (advice-add 'yes-or-no-p :around #'my-prevent-dialog-advice)
      (advice-add 'y-or-n-p :around #'my-prevent-dialog-advice)

      (if (file-directory-p "/opt/homebrew/bin")
          (setenv "PATH" (format "%s:%s" (getenv "PATH") "/opt/homebrew/bin")))
      (setenv "PATH" (format "%s:%s" (getenv "PATH") "/usr/local/bin"))
      (setenv "PATH" (format "%s:%s" (getenv "PATH") "~/.go/bin"))
      (setq exec-path (split-string (getenv "PATH") ":"))
      ;; fix drag n drop
      (global-set-key [C-M-s-drag-n-drop] 'ns-drag-n-drop)
      (setenv "LANG" "ja_JP.UTF-8")
      ))

; X用
(if (eq window-system 'x)
    (progn
      (define-key function-key-map [backspace] [8])
      (put 'backspace 'ascii-character 8)))

(if (not(eq window-system 'w32))
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp"))

(setq-default indent-tabs-mode nil)
(defalias 'yes-or-no-p 'y-or-n-p)

(straight-use-package 'use-package)
(setq use-package-verbose t)
(setq straight-use-package-by-default t)
(setq load-prefer-newer t)

;(setq debug-on-error t)

(use-package editorconfig :config (editorconfig-mode 1))

(use-package auto-highlight-symbol :config (global-auto-highlight-symbol-mode t))
;(use-package multiple-cursors :config (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))
;(use-package mode-icons :config (mode-icons-mode))
(use-package mmm-mode)
(use-package request)
(use-package graphql-mode)
(use-package ag :commands ag)
(use-package pt :commands pt)
(use-package dash)
(use-package inheritenv)
(use-package mise
  :init
  (setq mise-executable (expand-file-name "~/.local/bin/mise"))
  (add-hook 'after-init-hook #'global-mise-mode))
(use-package pipenv)
(use-package rhtml-mode)
(use-package coffee-mode)
(use-package hiwin
   :config
   ;; 選択中のフレームを強調
   (hiwin-activate)
   (set-face-background 'hiwin-face "gray8")
   )
(use-package nginx-mode)
(use-package rubocop)
(use-package groovy-mode)
(use-package dash-at-point
  :commands
  (dash-at-point dash-at-point-with-docset)
  :bind
  (("\C-cd" . dash-at-point)
   ("\C-ce" . dash-at-point-with-docset)))

(use-package wakatime-mode
  :config
  ;; wakatime-mode
  (if (and (executable-find "wakatime") (boundp 'wakatime-api-key))
      (global-wakatime-mode t)))
(use-package slim-mode)
(use-package terraform-mode)
(use-package vue-mode)
;(use-package prettier-js)
(use-package ng2-mode)
(with-eval-after-load 'typescript-mode (add-hook 'typescript-mode-hook #'lsp))

(use-package which-key
  :config
  (which-key-mode))

(use-package lua-mode)

;; リージョンをハイライト
;; C-g で解除(マークは残っているがリージョンは無効)
;; C-x C-x でリージョンを復活
;; M-; ハイライトがあればコメントアウト
(transient-mark-mode 1)

;; ウインドウ移動をShift+矢印で
(windmove-default-keybindings)

;; シンボリックは読め
(setq vc-follow-symlinks t)

(use-package visual-regexp
  :config
  (global-set-key (kbd "M-%") 'vr/query-replace))

;; 最近使ったファイルを時々保存
(use-package recentf
  :config
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '(".recentf" "^/ssh:"))
  (setq recentf-auto-cleanup 'never)

  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1)
  ;; recentf の メッセージをエコーエリア(ミニバッファ)に表示しない
  ;; (*Messages* バッファには出力される)
  (defun recentf-save-list-inhibit-message:around (orig-func &rest args)
    (setq inhibit-message t)
    (apply orig-func args)
    (setq inhibit-message nil)
    'around)
  (advice-add 'recentf-cleanup   :around 'recentf-save-list-inhibit-message:around)
  (advice-add 'recentf-save-list :around 'recentf-save-list-inhibit-message:around)
  )

;; カーソル位置の単語をハイライト
;; M-<left>	ahs-backward	前のシンボルへ移動
;; M-<right>	ahs-forward	次のシンボルへ移動
;; M-s-<left>	ahs-backward-difinition	?
;; M-s-<right>	ahs-forward-definition	?
;; M--	ahs-back-to-start	最初のカーソル位置のシンボルへ移動
;; C-x C-'	ahs-change-range	ハイライトする範囲を表示しているディスプレイの範囲かバッファ全体かを切り替える
;; C-x C-a	ahs-edit-mode	ハイライトしているシンボルを一括でrenameする
(use-package auto-highlight-symbol :config (global-auto-highlight-symbol-mode t))

;; 日本語インクリメンタル検索
(use-package migemo
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\g"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  
  ;; Set your installed path
  (if (eq window-system 'w32)
      (setq migemo-dictionary (expand-file-name "~/.emacs.d/share/migemo/dict/utf-8/migemo-dict")))
  (if (eq window-system 'ns)
      (if (file-directory-p "/opt/homebrew/bin")
          (setq migemo-dictionary "/opt/homebrew/opt/cmigemo/share/migemo/utf-8/migemo-dict")
        (setq migemo-dictionary "/usr/local/opt/cmigemo/share/migemo/utf-8/migemo-dict")))
  (message migemo-dictionary)
  (migemo-init)
  )

;; バックアップ関係
;; ロックファイルを作らない
(setq create-lockfiles nil)

;; backup-directory-alist は以下の構造を持つ
;; (regexp . directory)
;; regexp に一致したファイルのバックアップが directory に作られる
(setq backup-directory-alist '(("." . "~/.backup")))

(setq backup-by-copying t ;; don't clobber symlinks
      version-control t ;; Use version numbers for backups
      kept-new-versions 16 ;; Number of newest versions to keep
      kept-old-versions 2 ;; Number of oldest versions to keep
      delete-old-versions t ;; Ask to delete excess backup versions?
      backup-by-copying-when-linked t) ;; Copy linked files, don't rename.
(defun force-backup-of-buffer ()
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook  'force-backup-of-buffer)

;; http://namazu.org/~satoru/diary/?200203c&to=200203272#200203272
;; 編集中のファイルを開き直す
;; - yes/no の確認が不要;;   - revert-buffer は yes/no の確認がうるさい
;; - 「しまった! 」というときにアンドゥで元のバッファの状態に戻れる
;;   - find-alternate-file は開き直したら元のバッファの状態に戻れない
;;
(defun reopen-file ()
  (interactive)
  (let ((file-name (buffer-file-name))
        (old-supersession-threat
         (symbol-function 'ask-user-about-supersession-threat))
        (point (point)))
    (when file-name
      (fset 'ask-user-about-supersession-threat (lambda (fn)))
      (unwind-protect
          (progn
            (erase-buffer)
            (insert-file file-name)
            (set-visited-file-modtime)
            (goto-char point))
        (fset 'ask-user-about-supersession-threat
              old-supersession-threat)))))

;画面端でおりかえす
(setq truncate-partial-width-windows nil)

;; upcase[C-x C-u] downcase[C-x C-l] を問い合わせなしで実行
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Don't show Welcome Page
(setq inhibit-startup-message t)
;; Stop beep and flush
(setq ring-bell-function 'ignore)

;; (global-set-key "\C-h" 'backward-delete-char)
;; (global-set-key "\177" 'delete-char)
;goto-line はデフォルトでは M-g g
;(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-h" 'delete-backward-char)
;; TABをBackspaceで消す
(global-set-key [backspace] 'backward-delete-char)
;(global-set-key "\C-@" 'set-mark-command)

;ファイルの先頭に #! が含まれているとき、自動的に chmod +x を行ってくれます。
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(use-package projectile)

(use-package whitespace
  :commands whitespace-mode
  :hook
  ((ruby-mode . whitespace-mode)
   (haml-mode . whitespace-mode)
   (yaml-mode . whitespace-mode)
   (typescript-mode . whitespace-mode)
   (python-mode . whitespace-mode)
   (csharp-mode . whitespace-mode)
   )
  :config
  (setq whitespace-style '(face           ; faceで可視化
                           trailing       ; 行末
                           tabs           ; タブ
                           spaces         ; スペース
                           empty          ; 先頭/末尾の空行
                           space-mark     ; 表示のマッピング
                           tab-mark
                           ))

    ;; 全角スペース的なもの
  (setq alt-spaces "\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u200B\u3000\uFEFF")
  (setq space-marks
        (mapcar
         (lambda (c) (list 'space-mark c [?\u25a1]))
         alt-spaces))

  (setq whitespace-display-mappings
        (cl-concatenate
         'list
         space-marks
         ;; WARNING: the mapping below has a problem.
         ;; When a TAB occupies exactly one column, it will display the
         ;; character ?\xBB at that column followed by a TAB which goes to
         ;; the next TAB column.
         ;; If this is a problem for you, please, comment the line below.
         '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t]))))

  (setq whitespace-space-regexp (concat "\\([" alt-spaces "]+\\)"))

  ;; 保存前に自動でクリーンアップ
  (setq whitespace-action '(auto-cleanup))

  (let ((my/bg-color "black"))
    (set-face-attribute 'whitespace-trailing nil
                        :background my/bg-color
                        :foreground "DeepPink"
                        :underline t)
    (set-face-attribute 'whitespace-tab nil
                        :background my/bg-color
                        :foreground "LightSkyBlue"
                        :underline t)
    (set-face-attribute 'whitespace-space nil
                        :background my/bg-color
                        :foreground "GreenYellow"
                        :weight 'bold)
    (set-face-attribute 'whitespace-empty nil
                        :background my/bg-color))

  )

(use-package vertico
  :init
  (vertico-mode))

(use-package extensions/vertico-directory
  :straight (:type built-in)
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("C-l" . vertico-directory-up)
              ("\d" . vertico-directory-delete-char)))

(use-package consult
  :bind
  (
   ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   )
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  (setq consult-project-root-function #'projectile-project-root))

(defun consult-ripgrep-symbol-at-point ()
  (interactive)
  (consult-ripgrep nil (thing-at-point 'symbol)))

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  ; setting for lsp
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0) ;; default is 0.2  
  )

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :bind
  (:map copilot-completion-map
        ("<tab>" . 'copilot-accept-completion)
        ("TAB" . 'copilot-accept-completion)
        ("C-TAB" . 'copilot-accept-completion-by-word)
        ("C-<tab>" . 'copilot-accept-completion-by-word)
        ("M-]" . copilot-next-completion)
        ("M-[" . copilot-previous-completion))
  :init
  (setq warning-suppress-log-types '((copilot copilot-exceeds-max-char)))
  :hook
  ((prog-mode . copilot-mode))
  )

; dep key (setq openai-key "[YOUR API KEY]")
(use-package openai :straight (:host github :repo "emacs-openai/openai"))
(use-package chatgpt
  :straight (:host github :repo "emacs-openai/chatgpt")
  :config
  (setq chatgpt-model "chatgpt-4o-latest")
  )
(use-package codegpt :straight (:host github :repo "emacs-openai/codegpt"))
; (use-package dall-e :straight (:host github :repo "emacs-openai/dall-e"))


(use-package lsp-mode
  :hook
  ((web-mode . lsp)
   (scss-mode . lsp)
   (sass-mode . lsp)
   (vue-mode . lsp)
   (rustic-mode . lsp)
   (js-mode . lsp)
   (csharp-mode . lsp)
   (ruby-mode . lsp)
   (rbs-mode . lsp)
   )
  :config
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]vendor\\'")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]public\\'")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]tmp\\'")
  (setq
   lsp-restart 'ignore
   ;;lsp-log-io t
   lsp-log-max 1000
   read-process-output-max (* 1024 1024) ;; 1mb
   lsp-auto-guess-root t
   ))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq
   lsp-ui-sideline-show-diagnostics t
   lsp-ui-sideline-show-hover t
   )
  )

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (pipenv-mode)
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

;; (use-package tide
;;   :after (typescript-mode company flycheck)
;;   :hook ((typescript-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode)))

(use-package dap-mode
  :config
  ;(setq dap-auto-configure-features '(sessions locals controls tooltip))
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions tooltip))
  ;(setq dap-print-io t)
  (require 'dap-node)
  (dap-mode 1)
  (dap-auto-configure-mode 1)
  (dap-register-debug-template "TSX Attach"
                               (list :type "node"
                                     :request "attach"
                                     :name "TSX Attach"
                                     :port 9229
                                     :address "localhost"
                                     :localRoot "${workspaceFolder}"
                                     :remoteRoot "${workspaceFolder}"
                                     :skipFiles ["<node_internals>/**"]
                                     :sourceMaps t
                                     :outFiles ["${workspaceFolder}/**/*.js"]
                                     :protocol "inspector"))
  )


(use-package flycheck
  :config
  (flycheck-define-checker cfn-lint
    "AWS CloudFormation linter using cfn-lint.
Install cfn-lint first: pip install cfn-lint
See `https://github.com/aws-cloudformation/cfn-python-lint'."
    :command ("cfn-lint" "-f" "parseable" source)
    :error-patterns ((warning line-start (file-name) ":" line ":" column
                              ":" (one-or-more digit) ":" (one-or-more digit) ":"
                              (id "W" (one-or-more digit)) ":" (message) line-end)
                     (error line-start (file-name) ":" line ":" column
                            ":" (one-or-more digit) ":" (one-or-more digit) ":"
                            (id "E" (one-or-more digit)) ":" (message) line-end))
    :modes (cfn-yaml-mode))
  (add-to-list 'flycheck-checkers 'cfn-lint)
  (global-flycheck-mode))

;; リモートのファイルを編集するTRAMP
;; (use-package tramp
;;   :config
;;   (setq tramp-default-method "ssh"))

;;; Languages

;; vbnet-mode
(use-package vbnet-mode
  :mode "\\.\\(frm\\|bas\\|cls\\|vb\\)$"
  :load-path "./site-lisp"
  :config
  (turn-on-font-lock)
  (turn-on-auto-revert-mode)
  (setq vbnet-mode-indent 4)
  (setq vbnet-want-imenu t))

;; CSharp-mode
(add-hook
 'csharp-mode-hook
 '(lambda ()
    (dap-mode 1)
    (require 'dap-netcore)    
    ))

;; markdown-mode
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . gfm-mode))
  :init
  (setq markdown-gfm-additional-languages '("mermaid"))
  (setq markdown-command "pandoc")
  (setq markdown-open-command "marked2"))

(use-package websocket)
(use-package web-server)

(use-package maple-preview
  :straight (:host github :repo "honmaple/emacs-maple-preview" :files ("*.el" "index.html" "static"))
  :commands (maple-preview-mode)
  :config
  :init
  (setq maple-preview:allow-modes '(org-mode markdown-mode html-mode web-mode gfm-mode))
  (setq maple-preview:text-content '((t . maple-preview:markdown-content)))
  (setq maple-preview:js-file
        '(
          "/preview/static/js/jquery.min.js"
          "/preview/static/js/marked.min.js"
          "/preview/static/js/highlight.min.js"
          "<script type=\"module\">
import mermaid from \'https://cdn.jsdelivr.net/npm/mermaid@10.1.0/+esm\';
window.mermaid = mermaid;
</script>"))
  )

;; grep の結果画面は画面端で折り返さないけど、
;; コンパイルの結果画面は画面端で折り返す
(add-hook 'compilation-mode-hook
          '(lambda ()
	     (prin1 major-mode)
	     (if (member major-mode (list 'grep-mode 'ag-mode 'pt-mode))
		    (setq truncate-lines t))))



(use-package ruby-mode
  :mode "\\(\\.\\(rb\\|rake\\|ruby\\|thor\\|jbuilder\\|cap\\)\\|Gemfile\\)$"
  :config
  ;; hash-rocket を1.9記法に変換する
  (defun ruby-anti-hash-rocket ()
    (interactive)
    (beginning-of-line)
    (setq current-line (count-lines (point-min) (point)))
    (setq replaced (replace-regexp-in-string ":\\([a-z0-9_]+\\)\s*=>" "\\1:" (buffer-string)))
    (erase-buffer)
    (insert replaced)
    (goto-line (+ 1 current-line))
    (beginning-of-line)
    )
  )

(use-package rbs-mode)

;; HAML
;; C-i でインデント C-I でアンインデント
(use-package haml-mode
  :mode "\\.haml$"
  :config
  (c-set-offset 'substatement-open '0)
  (setq tab-width 8))


(use-package ggtags
  :commands ggtags-mode
  :init
  (setq ggtags-mode-hook
        '(lambda ()
           (local-set-key "\M-t" 'ggtags-find-definition) ;; 定義にジャンプ
           (local-set-key "\M-r" 'ggtags-find-reference)  ;; 参照を検索
           (local-set-key "\M-s" 'ggtags-find-symbol)     ;; 定義にジャンプ
	   (setq ggtags-completing-read-function nil)
	   ))
  (global-set-key "\M-e" 'ggtags-pop-stack)               ;; スタックを戻る
  :config
  ;; c-mode-common
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'web-mode)
                (ggtags-mode 1)))))

;; c-mod
(add-hook
 'c-mode-hook
 '(lambda ()
    ;; cedit
    (semantic-mode 1)
    (cpp-highlight-buffer t)
    (setq semantic-default-submodes
         '(
           global-semantic-idle-scheduler-mode
           global-semantic-idle-completions-mode
           global-semanticdb-minor-mode
           global-semantic-decoration-mode
           global-semantic-highlight-func-mode
           global-semantic-stickyfunc-mode
           global-semantic-mru-bookmark-mode
           ))
    (setq tab-width 4
          c-basic-offset 4
          indent-tabs-mode 1)

    (setq lsp-clients-clangd-args '("--header-insertion-decorators=0" "--log=verbose"))
    (lsp)
    ))

;; c++-mode
(add-hook
 'c++-mode-hook
 '(lambda ()
    ;; cedit
    (semantic-mode 1)
    (cpp-highlight-buffer t)
    (setq semantic-default-submodes
         '(
           global-semantic-idle-scheduler-mode
           global-semantic-idle-completions-mode
           global-semanticdb-minor-mode
           global-semantic-decoration-mode
           global-semantic-highlight-func-mode
           global-semantic-stickyfunc-mode
           global-semantic-mru-bookmark-mode
           ))
    ;; gtags
    (setq tab-width 4
          c-basic-offset 4
          indent-tabs-mode 1)))

;; add-node-modules-path
(use-package add-node-modules-path
  :hook
  ((js-mode . add-node-modules-path)
   (typescript-mode . add-node-modules-path)
   (web-mode . add-node-modules-path)
   (vue-mode . add-node-modules-path))
  :commands
  add-node-modules-path)

(use-package kotlin-mode
  :mode "\\.kt$")

;; YAML-mode
(use-package yaml-mode
  :mode "\\.yml$"
  :config
  (define-derived-mode cfn-yaml-mode yaml-mode
    "CFN-YAML"
    "Simple mode to edit CloudFormation template in YAML format.")
  (add-to-list 'magic-mode-alist
               '("\\(---\n\\)?AWSTemplateFormatVersion:" . cfn-yaml-mode)))

(use-package yaml-tomato
  :commands
  (yaml-tomato-show-current-path yaml-tomato-copy))

;; css-mode
(add-hook 'css-mode-hook
	  '(lambda ()
	     (setq css-indent-offset 2)))

;; scss-mode
(use-package scss-mode
  :mode "\\.scss$"
  :config
  (setq scss-compile-at-save nil))

;; sass-mode
(use-package sass-mode
  :mode "\\.sass\\'")

;; html-mode
(add-hook 'html-mode-hook
          (lambda()
            (setq sgml-basic-offset 2)))

(use-package add-node-modules-path
  :hook
  ((typescript-mode . add-node-modules-path)))

;; js-mode (javascript-mode)
;; (add-hook 'js-mode-hook
;;           (lambda ()
;;             (setq js-indent-level 2)))

(use-package typescript-mode
  :mode "\\.m?ts$"
  :config
  (setq lsp-clients-angular-language-server-command
        (list
         "/ngserver"
         "--tsProbeLocations" "./node_modules"
         "--ngProbeLocations" "./node_modules/@angular"
         "--stdio"
         ))
  )

(defun my-typescript-mode-setup ()
  (when (string= (file-name-extension buffer-file-name) "ts")
    (if (string-match-p "\\.component\\.ts$" (file-name-nondirectory buffer-file-name))
        (lsp 'angular)  ;; Angular LSPを起動
      (lsp))))          ;; 通常のTypeScript LSPを起動
(add-hook 'typescript-mode-hook #'my-typescript-mode-setup)


(if (or (eq window-system 'w32) (eq window-system 'ns) (eq window-system 'x))
    (progn
      (use-package server
        ;; クライアントを終了するとき終了するかどうかを聞かない
        ;; サーバ起動
        :config
        (unless (server-running-p)
          (progn
            (server-start)
            (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))))))

;;web-mode
(use-package web-mode
  :mode (("\\.\\(ctp\\|php\\|php5\\|inc\\)$" . web-mode)
         ("\\.phtml$"     . web-mode)
         ("\\.tpl\\.php$" . web-mode)
         ("\\.jsp$"       . web-mode)
         ("\\.as[cp]x$"   . web-mode)
         ("\\.cshtml$"   . web-mode)
         ("\\.erb$"       . web-mode)
         ("\\.ctp$"     . web-mode)
         ("\\.tsx$"     . web-mode))
  :config
  (setq web-mode-enable-auto-indentation nil)
  (setq web-mode-enable-auto-quoting nil))

(use-package haskell-mode
  :mode
  (("\\.hs$" . haskell-mode)
   ("\\.lhs$" . literate-haskell-mode)
   ("\\.cabal\\'" . haskell-cabal-mode))
  :commands
  (haskell-mode literate-haskell-mode haskell-cabal-mode)
  :hook
  haskell-indentation-mode)

(use-package dockerfile-mode
  :mode "[Dd]ockerfile")

;; elixir-mode
;(use-package alchemist)
;(use-package ac-alchemist)
;(use-package elixir-mode
;  :init
;  (alchemist-mode)
;  (ac-alchemist-setup))

;; rust
(use-package rustic
  :mode ("\\.rs$" . rustic-mode)
  :commands rustic-mode
  :config
  )

(use-package fsharp-mode
  :mode "\\.fs[iylx]?$")

(use-package ansi-color
  :config
  (defun display-ansi-colors ()
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max))))

(use-package csv-mode
  :mode ("\\.csv$" . csv-align-mode))

(setq ediff-split-window-function 'split-window-horizontally)

(defun projectile-path ()
  (file-relative-name buffer-file-name (projectile-project-root)))

(defun copy-projectile-path ()
  (interactive)
  (kill-new (projectile-path)))

(defun copy-projectile-line ()
  (interactive)
  (kill-new
   (concat (projectile-path) ":" (number-to-string (line-number-at-pos)))))

(put 'dired-find-alternate-file 'disabled nil)
