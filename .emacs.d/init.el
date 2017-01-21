;; ↓等幅フォントチェック用
;; 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
;; あいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえおあいうえお
;; メモ
;; 現在有効なキーボードショートカットを表示するには<F1> b
;;
;; .emacs.el を再読み込みするには
;; C-x C-s または Command + s
;; M-x load-file RET ~/.emacs.el RET
;;
;; Dependencies
;;    marked (npmで入れる)
;;    pry
;;    pry-doc
;;    method_source
;;    rubocop
;;    ruby-lint
;;    cmigemo (日本語検索)
;;    ※ migemo のインストール時に文字コードエラーが出るので euc-jp-unix を選んでやること

;;; load-path の追加
(defun add-load-path (path)
  (setq path (expand-file-name path))
  (unless (member path load-path)
    (add-to-list 'load-path path)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t) ;; MELPAを追加
(package-initialize)

;; インストールするパッケージ
(defvar my/favorite-packages
  '(
    auto-complete
    robe
    helm
    helm-ag
    helm-swoop
    migemo
    rvm
    rbenv
;    yasnippet
    enh-ruby-mode
    inf-ruby
    rhtml-mode
    web-mode
    js2-mode
    yaml-mode
    haml-mode
    markdown-mode
    coffee-mode
    scss-mode
    sass-mode
    ggtags
    ag ; projectile-ag で必要
    pt
    projectile
    helm-projectile
    helm-ghq
    hiwin
    haskell-mode
    dockerfile-mode
    nginx-mode
    rubocop
    flycheck
    flycheck-pos-tip
    auto-highlight-symbol
    csharp-mode
    visual-regexp
    ))

;; my/favorite-packagesからインストールしていないパッケージをインストール
(setq my-packages-loaded nil)
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (unless my-packages-loaded
      (package-refresh-contents);; パッケージ情報の更新
      (setq my-packages-loaded t))
    (package-install package)))

(defun set-font-size (height)
  (interactive "nHeight:")
  (set-face-attribute 'default (selected-frame) :height height))

;; Emacs
;; GUIの設定が後から動くとなんかうざい感じになるので先に動かす
(if (eq window-system 'w32)
    (progn
      (custom-set-variables
       '(column-number-mode t)
       '(show-paren-mode t)
       '(tool-bar-mode nil))
      (custom-set-faces
       '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "outline" :family "Osaka－等幅")))))
      (set-frame-parameter nil 'alpha 85)
      (setq default-frame-alist
	    (append (list
;; 		     '(foreground-color . "white")
;; 		     '(background-color . "black")
;; 		     '(border-color . "black")
;; 		     '(mouse-color . "red")    ; ???
;; 		     '(cursor-color . "white") ;
 		     '(width . 120)     ; フレームの横幅
 		     '(height . 50)    ; フレームの高さ
;; 		     '(alpha . 85)
 		     )default-frame-alist))
      ;; デフォルトの文字コードはUTF-8にする
      (set-default-coding-systems 'utf-8)
      (prefer-coding-system 'utf-8-unix)

      (setq grep-command "grep -n -e ")
      (setq grep-program "grep")))

;; Homebrew emacs-app 24用
(if (or (eq window-system 'ns)
	(eq window-system 'mac))
    (progn
      ;; フレームのディフォルトの設定。
      (custom-set-variables
       '(column-number-mode t)
       '(show-paren-mode t)
       '(tool-bar-mode nil))

      (set-fontset-font "fontset-standard"
                        'ascii
                        (font-spec :family "ricty" :size 13) nil 'prepend)  ;; ここでサイズを指定
      (set-fontset-font "fontset-standard"
                        'japanese-jisx0213.2004-1
                        (font-spec :family "ricty") nil 'prepend)

      (setq default-frame-alist
	    (append (list
		     '(foreground-color . "white")
		     '(background-color . "black")
		     '(width . 120)     ; フレームの横幅
		     '(height . 50)    ; フレームの高さ
		     '(alpha . 85)
		     '(font . "fontset-standard")
		     )default-frame-alist))

      ;; コマンドから open -a Emacs.app されたときに新しいフレームを開かない
      (setq ns-pop-up-frames nil)

      ;; MacPorts のemacs-app はデフォルトでMacのキーバインド使える
      ;; C-zで最小化してうざいので無効に
      (global-unset-key "\C-z")

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
      (setq frame-title-format (format "%%f - Emacs@%s" (system-name)))

      ;; disable x-popup-dialog
      (defadvice yes-or-no-p (around prevent-dialog activate)
	"Prevent yes-or-no-p from activating a dialog"
	(let ((use-dialog-box nil))
	  ad-do-it))
      (defadvice y-or-n-p (around prevent-dialog-yorn activate)
	"Prevent y-or-n-p from (and )ctivating a dialog"
	(let ((use-dialog-box nil))
	  ad-do-it))

      (setenv "PATH" (format "%s:%s" (getenv "PATH") "/usr/local/bin"))
      (setenv "PATH" (format "%s:%s" (getenv "PATH") "~/.go/bin"))
      (setq exec-path (split-string (getenv "PATH") ":"))))

; X用
(if (eq window-system 'x)
    (progn
      (define-key function-key-map [backspace] [8])
      (put 'backspace 'ascii-character 8)))

(if (not(eq window-system 'w32))
  (add-load-path "/usr/local/share/emacs/site-lisp"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

;; リージョンをハイライト
;; C-g で解除(マークは残っているがリージョンは無効)
;; C-x C-x でリージョンを復活
;; M-; ハイライトがあればコメントアウト
(transient-mark-mode 1)

;; ウインドウ移動をShift+矢印で
(windmove-default-keybindings)

;; visual-regexp
(global-set-key (kbd "M-%") 'vr/query-replace)

;; 最近使ったファイルを時々保存
(when (require 'recentf nil t)
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1))

;; カーソル位置の単語をハイライト
;; M-<left>	ahs-backward	前のシンボルへ移動
;; M-<right>	ahs-forward	次のシンボルへ移動
;; M-s-<left>	ahs-backward-difinition	?
;; M-s-<right>	ahs-forward-definition	?
;; M--	ahs-back-to-start	最初のカーソル位置のシンボルへ移動
;; C-x C-'	ahs-change-range	ハイライトする範囲を表示しているディスプレイの範囲かバッファ全体かを切り替える
;; C-x C-a	ahs-edit-mode	ハイライトしているシンボルを一括でrenameする
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)

;; 日本語インクリメンタル検索
(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))

;; Set your installed path
(if (eq window-system 'w32)
    (setq migemo-dictionary (expand-file-name "~/.emacs.d/share/migemo/dict/utf-8/migemo-dict")))
(if (or (eq window-system 'ns) (eq window-system 'mac))
    (setq migemo-dictionary "/usr/local/Cellar/cmigemo/HEAD/share/migemo/utf-8/migemo-dict"))

(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(migemo-init)

;; バックアップ関係

;; backup-directory-alist は以下の構造を持つ
;; (regexp . directory)
;; regexp に一致したファイルのバックアップが directory に作られる
(setq backup-directory-alist (cons (cons "\\.*$" (expand-file-name "~/.backup"))
				   backup-directory-alist))

(setq version-control t ;; Use version numbers for backups
      kept-new-versions 16 ;; Number of newest versions to keep
      kept-old-versions 2 ;; Number of oldest versions to keep
      delete-old-versions t ;; Ask to delete excess backup versions?
      backup-by-copying-when-linked t) ;; Copy linked files, don't rename.
(defun force-backup-of-buffer ()
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook  'force-backup-of-buffer)

;; シンボリックファイルはリンク先を自動で開く
(custom-set-variables '(vc-follow-link t))

;; 自動でrevert-buffer
;; ↓こいつをnon-nilにしておくと、vcsによる変更もチェックしてくれる
;; (setq auto-revert-check-vc-info t)
;; (setq auto-revert-interval 30)
;; (add-hook 'find-file-hook
;; 	  '(lambda ()
;; 	     (when (and buffer-file-name
;; 			(vc-backend buffer-file-name))
;; 	       (auto-revert-mode))))



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

;; リージョンの行数を表示
(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines,%d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
      ;;(count-lines-region (region-beginning) (region-end)) ;; これだとｴｺｰｴﾘｱがﾁﾗつく
    ""))
(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))

;画面端でおりかえす
(setq truncate-partial-width-windows nil)

;; 対応する括弧を光らせる
(show-paren-mode t)

;; upcase[C-x C-u] downcase[C-x C-l] を問い合わせなしで実行
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Don't show Welcome Page
(setq inhibit-startup-message t)
;; Stop beep and flush
(setq ring-bell-function 'ignore)

 ;Show column number
(column-number-mode t)

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

(when (require 'helm-config nil t)
  (helm-mode 1)
  (helm-migemo-mode 1)
  (define-key global-map (kbd "C-;") 'helm-mini)
  (define-key global-map (kbd "M-x")     'helm-M-x)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  ;; helm-swoop
  (global-set-key (kbd "M-i") 'helm-swoop)
  (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

  ;; For helm-find-files etc.
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

  ;helm でC-k の挙動を通常のバッファと同等にする
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  (require 'helm-buffers)
  (defadvice helm-buffers-sort-transformer (around ignore activate)
    (setq ad-return-value (ad-get-arg 0)))

  ;; helm-ag
  (if (eq window-system 'w32)
      (setq helm-ag-base-command "pt -e --nocolor --nogroup")) ;; Windows はShift-JISとか来るのでpt
  (if (eq window-system 'ns)
      (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case"))

  (setq helm-ag-command-option "--all-text")
  (setq helm-ag-insert-at-point 'symbol)
  (defun projectile-helm-ag ()
    (interactive)
    (helm-ag (projectile-project-root)))

  ;; helm-projectile
  (helm-projectile-on)

  (setq helm-mini-default-sources
    '(helm-source-projectile-files-list
      helm-source-projectile-projects
      helm-source-recentf
      helm-source-buffer-not-found))

  )

(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         spaces         ; スペース
                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))

(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        ;; WARNING: the mapping below has a problem.
        ;; When a TAB occupies exactly one column, it will display the
        ;; character ?\xBB at that column followed by a TAB which goes to
        ;; the next TAB column.
        ;; If this is a problem for you, please, comment the line below.
        (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

;; スペースは全角のみを可視化
(setq whitespace-space-regexp "\\(\u3000+\\)")

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


;; auto-complete
(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories (expand-file-name "~/.emacs.d/auto-complete/dict"))
(ac-config-default)
;; これを設定するとC-n C-p で候補の選択ができるようになる.
(setq ac-use-menu-map t)

;; flycheck
(global-flycheck-mode)
(flycheck-pos-tip-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(custom-set-variables
 '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs)))

;(define-key ac-menu-map "\C-n" 'ac-next)
;(define-key ac-menu-map "\C-p" 'ac-previous)

;; リモートのファイルを編集するTRAMP
(require 'tramp)
(setq tramp-default-method "ssh")

;; CSharp-mode
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
(add-hook
 'csharp-mode-hook
 '(lambda ()
    (c-set-offset 'substatement-open '0)
    (setq tab-width  4
          c-basic-offset 4
          indent-tabs-mode nil)))

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(add-hook 'markdown-mode-hook
	  '(lambda()
	     (setq markdown-command "/usr/local/bin/multimarkdown")
	     (setq markdown-open-command "marked2")))


;; grep の結果画面は画面端で折り返さないけど、
;; コンパイルの結果画面は画面端で折り返す
(add-hook 'compilation-mode-hook
          '(lambda ()
	     (prin1 major-mode)
	     (if (member major-mode (list 'grep-mode 'ag-mode))
		    (setq truncate-lines t))))

;; Coffee-mode
(autoload 'coffee-mode "coffee-mode" "Major mode for editing Coffeescript." t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(defun coffee-custom ()
  "coffee-mode-hook"
  (setq indent-tabs-mode nil)
  (set (make-local-variable 'tab-width) 8)
  (setq coffee-tab-width 2))
(add-hook 'coffee-mode-hook
  '(lambda()
     (coffee-custom)))

;; ruby-mode
(if (or (eq window-system 'ns) (eq window-system 'mac)) ;;何故かターミナルで動かない そのうち調べる
    (progn
      (if (string-match "not found" (shell-command-to-string "which rbenva"))
          (progn
            (require 'rvm)
            (rvm-use-default))
        (progn
          (require 'rbenv)
          (global-rbenv-mode)))))

(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(add-to-list 'auto-mode-alist '("\\.\\(rb\\|rake\\|ruby\\)$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\.builder$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(setq interpreter-mode-alist (append '(("ruby" . enh-ruby-mode))
  interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")

;; 保存時にmagic commentを追加しないようにする
(defadvice enh-ruby-mode-set-encoding (around stop-enh-ruby-mode-set-encoding)
  "If enh-ruby-not-insert-magic-comment is true, stops enh-ruby-mode-set-encoding."
  (if (and (boundp 'enh-ruby-not-insert-magic-comment)
           (not enh-ruby-not-insert-magic-comment))
      ad-do-it))
(ad-activate 'enh-ruby-mode-set-encoding)
(setq-default enh-ruby-not-insert-magic-comment t)

(add-hook 'enh-ruby-mode-hook
	  '(lambda ()
	     (inf-ruby-minor-mode)
	     (auto-complete-mode)
	     (robe-mode)
	     (projectile-mode)
	     (make-local-variable 'ac-ignores)
	     (add-to-list 'ac-ignores "end")
	     (whitespace-mode)
	     (rvm-activate-corresponding-ruby)
	     ;; flycheck とrubocop で Rails を有効にする
	     (setq rubocop-check-command "rubocop --format emacs -R")
	     (flycheck-define-checker ruby-rubocop
	       "A Ruby syntax and style checker using the RuboCop tool.

See URL `http://batsov.com/rubocop/'."
	       :command ("rubocop" "--display-cop-names" "--format" "emacs" "-R"
			 (config-file "--config" flycheck-rubocoprc)
			 (option-flag "--lint" flycheck-rubocop-lint-only)
			 source)
	       :error-patterns
	       ((info line-start (file-name) ":" line ":" column ": C: "
		      (optional (id (one-or-more (not (any ":")))) ": ") (message) line-end)
		(warning line-start (file-name) ":" line ":" column ": W: "
			 (optional (id (one-or-more (not (any ":")))) ": ") (message)
			 line-end)
		(error line-start (file-name) ":" line ":" column ": " (or "E" "F") ": "
		       (optional (id (one-or-more (not (any ":")))) ": ") (message)
		       line-end))
	       :modes (enh-ruby-mode ruby-mode)
	       :next-checkers ((warning . ruby-rubylint)))

	     (setq flycheck-checker 'ruby-rubocop)
             (flycheck-mode 1)
	     ))

(add-hook 'robe-mode-hook 'ac-robe-setup)

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

;; HAML
;; C-i でインデント C-I でアンインデント
(autoload 'haml-mode "haml-mode" "Mode for editing HAML" t)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(add-hook
 'haml-mode-hook
 '(lambda ()
    (add-to-list 'write-file-functions 'delete-trailing-whitespace)
    (c-set-offset 'substatement-open '0)
    (rvm-activate-corresponding-ruby)
    (setq tab-width  8
          indent-tabs-mode nil)))

(require 'rhtml-mode)
(add-hook 'haml-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)))

;; Scheme-mode
(setq scheme-program-name "gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)


(autoload 'ggtags-mode "ggtags" "" t)
(setq ggtags-mode-hook
      '(lambda ()
         (local-set-key "\M-t" 'ggtags-find-definition) ;; 定義にジャンプ
         (local-set-key "\M-r" 'ggtags-find-reference)  ;; 参照を検索
         (local-set-key "\M-s" 'ggtags-find-symbol)     ;; 定義にジャンプ
	 (setq ggtags-completing-read-function nil)
	 (helm-mode 1)
	 ))
(global-set-key "\M-e" 'ggtags-pop-stack)               ;; スタックを戻る

;; c-mode-common
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'web-mode)
              (ggtags-mode 1))))


;; c-mod
(add-hook
 'c-mode-hook
 '(lambda ()
    ;; cedit
    (semantic-mode 1)
    (cpp-highlight-buffer t)
    (projectile-mode)
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
          indent-tabs-mode 1)))

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

;; objc-mode
(add-hook
 'objc-mode-hook
 '(lambda ()
    (c-set-offset 'substatement-open '0)
    (setq tab-width 4)
    (c-basic-offset 4)
    (indent-tabs-mode 1)))


;; YAML-mode
(autoload 'yaml-mode "yaml-mode" "YAML mode" t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode nil)))
;; css-mode
(add-hook 'css-mode-hook
	  '(lambda ()
	     (rvm-activate-corresponding-ruby)
	     (setq css-indent-offset 2)
	     (setq indent-tabs-mode nil)
	     (setq css-indent-offset 2)
	     ))

;; scss-mode
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(setq scss-compile-at-save nil)

;; sass-mode
(autoload 'sass-mode "sass-mode")
(add-to-list 'auto-mode-alist '("\\.sass\\'" . sass-mode))

;; html-mode
(add-hook 'html-mode-hook
          (lambda()
            (setq sgml-basic-offset 2)
            (setq indent-tabs-mode nil)))

;; delphi-mode
(add-to-list 'auto-mode-alist '("\\.pas$" . delphi-mode))
(add-to-list 'auto-mode-alist '("\\.dpr$" . delphi-mode))

(add-hook 'delphi-mode-hook
	  #'(lambda ()
	      (custom-set-variables
	       '(delphi-indent-level 2)
	       '(delphi-case-label-indent 2))
	      (setq comment-start "// ")
	      (loop for c from ?! to ?' do (modify-syntax-entry  c "."))
	      (loop for c from ?* to ?/ do (modify-syntax-entry  c "."))
	      (loop for c from ?: to ?@ do (modify-syntax-entry  c "."))
	      (modify-syntax-entry  ?\ ".")
	      (modify-syntax-entry  ?^ ".")
	      (modify-syntax-entry  ?` ".")
	      (modify-syntax-entry  ?~ ".")
	      (modify-syntax-entry  ?| ".")
	      (local-set-key (kbd "<RET>")
			     #'(lambda ()
				 (interactive)
				 (indent-according-to-mode)
				 (newline-and-indent)))
                 ;; (turn-on-lazy-lock)
	      (add-hook 'compilation-mode-hook
			#'(lambda ()
			    (add-to-list 'compilation-error-regexp-alist
					 '("^\\([^(]+\\)(\\([0-9]+\\)" 1 2))))
	      (add-hook 'speedbar-mode-hook
			#'(lambda ()
			    (setq speedbar-file-unshown-regexp
				  (concat
				   speedbar-file-unshown-regexp
				   "\\|\\.dfm\\|\\.ddp\\|\\.dcu\\|\\.dof"))
			    (speedbar-add-supported-extension ".pas")))

	      (abbrev-mode 1)
	      (define-abbrev local-abbrev-table
		"beg" t #'(lambda ()
			    (skeleton-insert '(nil "in" > \n
						   _ \n
						   "end;" > \n))
			    (setq skeleton-abbrev-cleanup (point))
			    (add-hook 'post-command-hook
				      'skeleton-abbrev-cleanup
				      nil t)))
	      (define-abbrev local-abbrev-table
		"bege" t #'(lambda ()
			     (skeleton-insert '(nil -1 "in" > \n
						    _ \n
						    "end" > \n
						    "else" > \n
						    "begin" > \n \n
						    "end;" > \n))
			     (setq skeleton-abbrev-cleanup (point))
			     (add-hook 'post-command-hook
				       'skeleton-abbrev-cleanup
				       nil t)))
	      (define-abbrev local-abbrev-table
		"if" t #'(lambda ()
			   (skeleton-insert '(nil _ " then" > \n))))
	      (define-abbrev local-abbrev-table
		"ife" t #'(lambda ()
			    (skeleton-insert '(nil -1 _ " then" > \n \n
						   "else" > \n))))
	      (define-abbrev local-abbrev-table
		"ifb" t #'(lambda ()
			    (skeleton-insert '(nil -1 _ " then" > \n
						   "begin" > \n \n
						   "end;" > \n))))
	      (define-abbrev local-abbrev-table
		"ifbe" t #'(lambda ()
			     (backward-delete-char 1)
			     (skeleton-insert '(nil -1 _ " then" > \n
						    "begin" > \n \n
						    "end" > \n
						    "else" > \n
						    "begin" > \n \n
						    "end;" > \n))))
	      (define-abbrev local-abbrev-table
		"proc" t #'(lambda ()
			     (skeleton-insert '(nil "edure" _ ";" > \n
						    "var" > \n \n
						    "begin" > \n \n
						    "end;" > \n))))
	      (define-abbrev local-abbrev-table
		"func" t #'(lambda ()
			     (skeleton-insert '(nil "tion" _ " : ;" > \n
						    "var" > \n \n
						    "begin" > \n \n
						    "end;" > \n))))
	      (define-abbrev local-abbrev-table
		"for" t #'(lambda ()
			    (skeleton-insert '(nil _ " to do" > \n))))
	      (define-abbrev local-abbrev-table
		"forb" t #'(lambda ()
			     (skeleton-insert '(nil -1 _ " to do" > \n
						    "begin" > \n \n
						    "end;" > \n))))
	      ))

(defvar imenu--function-name-regexp-delphi
  (concat
   "^[ \t]*\\(function\\|procedure\\|constructor\\|destructor\\)[ \t]+"
   "\\([_a-zA-Z][_a-zA-Z0-9]*\\.\\)?"   ; class?
   "\\([_a-zA-Z][_a-zA-Z0-9]*\\)")
  "Re to get function/procedure names in Delphi.")

(defun imenu--create-delphi-index (&optional regexp)
  (let ((index-alist '())
	(progress-prev-pos 0)
	(case-fold-search t))
    (goto-char (point-min))
    (imenu-progress-message progress-prev-pos 0)
    (save-match-data
      (while (re-search-forward
	      (or regexp imenu--function-name-regexp-delphi)
	      nil t)
	(imenu-progress-message progress-prev-pos)
	(let ((pos (save-excursion
		     (beginning-of-line)
		     (if imenu-use-markers (point-marker) (point))))
	      (function-name (match-string-no-properties 3)))
	  (push (cons function-name pos)
		index-alist))))
    (imenu-progress-message progress-prev-pos 100)
    (nreverse index-alist)))

(add-hook 'delphi-mode-hook
	  #'(lambda ()
	      (require 'imenu)
	      (setq imenu-create-index-function
		    #'imenu--create-delphi-index)
	      (imenu-add-menubar-index)))

;; js2-mode
(autoload 'js2-mode "js2-mode" "JS2 mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.es6$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-hook 'js2-mode-hook
	  '(lambda ()
	     (flycheck-mode 1)
	     (setq js2-include-browser-externs nil)
	     (setq js2-mode-show-parse-errors nil)
	     (setq js2-mode-show-strict-warnings nil)
	     (setq js2-highlight-external-variables nil)
	     (setq js2-include-jslint-globals nil)
	     (setq c-basic-offset 2)
	     (setq indent-tabs-mode nil)
	     (setq js2-basic-offset 2)))


(if (or (eq window-system 'w32) (eq window-system 'mac) (eq window-system 'ns) (eq window-system 'x))
    (progn
      ;; クライアントを終了するとき終了するかどうかを聞かない
      ;; サーバ起動
      (require 'server)
      (unless (server-running-p)
	(progn
	  (server-start)
	  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)))))

;; 選択中のフレームを強調
(hiwin-activate)
(set-face-background 'hiwin-face "gray8")

;;web-mode
(require 'web-mode)
;; PHP-mode
(setq web-mode-indent 4)
(autoload 'php-mode "web-mode" "web-mode" t)
(add-to-list 'auto-mode-alist '("\\.\\(ctp\\|php\\|php5\\|inc\\)$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp$"     . web-mode))
(add-hook
 'web-mode-hook
 '(lambda ()
    ;(setq-default indent-tabs-mode nil)
    (setq indent-tabs-mode t)
    (setq-default tab-width 4)
    (setq web-mode-markup-indent-offset web-mode-indent)
    (setq web-mode-css-indent-offset web-mode-indent)
    (setq web-mode-code-indent-offset web-mode-indent)
    (setq web-mode-style-padding 0)
    (setq web-mode-script-padding 0)
    (setq web-mode-block-padding 0)
    (setq web-mode-comment-style web-mode-indent)))

;; haskell-mode
(autoload 'haskell-mode "haskell-mode" nil t)
(autoload 'haskell-cabal "haskell-cabal" nil t)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))
;; indent の有効.
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

;; dockerfile-mode
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(if (file-exists-p "~/.custom.el")
    (load "~/.custom.el"))
