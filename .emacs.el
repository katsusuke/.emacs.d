;; .emacs.el
;; last update 2012/4/17
;; メモ
;; 現在有効なキーボードショートカットを表示するには<F1> b
;;
;; .emacs.el を再読み込みするには
;; C-x C-s または Command + s
;; M-x load-file RET ~/.emacs.el RET

;; Emacs
;; GUIの設定が後から動くとなんかうざい感じになるので先に動かす
(if (eq window-system 'w32)
    (progn
      (custom-set-variables
       '(column-number-mode t)
       '(show-paren-mode t)
       '(tool-bar-mode nil))
      (custom-set-faces
       '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "outline" :family "Osaka－等幅"))))
       '(wb-line-number-face ((t (:foreground "LightGrey"))))
       '(wb-line-number-scroll-bar-face
	 ((t (:foreground "white" :background "LightBlue2")))))
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

      ;; 静的検証作業用
      (setenv "PATH" (format "c:\\cygwin\\bin;%s" (getenv "PATH")))
      (setenv "PATH" (format "c:\\cygwin\\usr\\local\\bin;%s" (getenv "PATH")))
      (setenv "PATH" (format "c:\\cygwin64\\bin;%s" (getenv "PATH")))
      (setenv "PATH" (format "c:\\cygwin64\\usr\\bin;%s" (getenv "PATH")))
      (setenv "PATH" (format "c:\\cygwin64\\usr\\local\\bin;%s" (getenv "PATH")))

      (setenv "CYGWIN" "nodosfilewarning")
;      (setq find-grep-options " | sed 's/^\\/cygdrive\\/\\([a-z]\\)/\\1:/g'")
      (setq grep-command "grep -n -e ")
      (setq grep-program "grep")))

;; Carbon Emacs 22用
(if (eq window-system 'mac)
    (progn
      ;; フレームのディフォルトの設定。
      (custom-set-variables
       '(column-number-mode t)
       '(show-paren-mode t)
       '(tool-bar-mode nil))
      (custom-set-faces
       ;; custom-set-faces was added by Custom.
       ;; If you edit it by hand, you could mess it up, so be careful.
       ;; Your init file should contain only one such instance.
       ;; If there is more than one, they won't work right.
       '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "outline")))))
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
      
      (require 'carbon-font)
      ; Hide menu bar and tool bar
      ;(setq menu-bar-mode nil)
      ;(tool-bar-mode nil)

      (fixed-width-set-fontset "hiramaru" 10)

      ;; 最近使ったファイル
      (recentf-mode t)
      (setq recentf-max-menu-items 10)
      (setq recentf-max-saved-items 20)
      ;; (setq recentf-exclude '("^/[^/:]+:")) ;除外するファイル名

      ;; Macのキーバインドを使う
      ;; キーバインドの一覧はこちら
      ;; → http://macwiki.sourceforge.jp/wiki/index.php/MacKeyMode
      (mac-key-mode 1)
      ;; C-zで最小化してうざいので無効に
      (global-unset-key "\C-z")
      ;; Option キーを Meta キーとして使う
      (setq mac-option-modifier 'meta)
      ;; C-x <left/right>でバッファの切り替え（これはデフォルト動作）
      ;; C-x <up/down>でフレームの切り替え
      (global-set-key [?\C-x up] '(lambda () "" (interactive) (other-frame -1)))
      (global-set-key [?\C-x down] '(lambda () "" (interactive) (other-frame 1)))

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

      ;; スクロールゆっくり
      (global-set-key [wheel-up] '(lambda () "" (interactive) (scroll-down 1)))
      (global-set-key [wheel-down] '(lambda () "" (interactive) (scroll-up 1)))
      (global-set-key [double-wheel-up] '(lambda () "" (interactive) (scroll-down 2)))
      (global-set-key [double-wheel-down] '(lambda () "" (interactive) (scroll-up 2)))
      (global-set-key [triple-wheel-up] '(lambda () "" (interactive) (scroll-down 3)))
      (global-set-key [triple-wheel-down] '(lambda () "" (interactive) (scroll-up 3)))

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
      ;;(setq hl-line-face 'underline)
      (global-hl-line-mode)

      ;; Show filename on titlebar
      (setq frame-title-format (format "%%f - Emacs@%s" (system-name)))
))
;; MacPorts emacs-app 24用
(if (eq window-system 'ns)
    (progn
      (defun set-frame-default ()
	;; フレームのディフォルトの設定。
	(custom-set-variables
	 '(column-number-mode t)
	 '(show-paren-mode t)
	 '(tool-bar-mode nil))
	(custom-set-faces
	 ;; custom-set-faces was added by Custom.
	 ;; If you edit it by hand, you could mess it up, so be careful.
	 ;; Your init file should contain only one such instance.
	 ;; If there is more than one, they won't work right.
	 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "outline")))))
	;(set-frame-parameter nil 'alpha 85)
	(setq default-frame-alist
	      (append (list 
		       ;; 		     '(foreground-color . "white")
		       ;; 		     '(background-color . "black")
		       ;; 		     '(border-color . "black")
		       ;; 		     '(mouse-color . "red")    ; ???
		       ;; 		     '(cursor-color . "white") ;
		       '(width . 120)     ; フレームの横幅
		       '(height . 50)    ; フレームの高さ
		       '(alpha . 85)
		       )default-frame-alist))
	;; フォント設定 :height を変えるとサイズが変わる
	;; 他をいじるとカオス
	(set-face-attribute 'default nil :family "monaco" :height 100)
	(set-fontset-font
	 (frame-parameter nil 'font)
	 'japanese-jisx0208
	 '("Hiragino Kaku Gothic ProN" . "iso10646-1"))
	(set-fontset-font
	 (frame-parameter nil 'font)
	 'japanese-jisx0212
	 '("Hiragino Kaku Gothic ProN" . "iso10646-1")) 
	(set-fontset-font
	 (frame-parameter nil 'font)
	 'mule-unicode-0100-24ff
	 '("monaco" . "iso10646-1"))
	(setq face-font-rescale-alist
	      '(("^-apple-hiragino.*" . 1.2)
		(".*osaka-bold.*" . 1.2)
		(".*osaka-medium.*" . 1.2)
		(".*courier-bold-.*-mac-roman" . 1.0)
		(".*monaco cy-bold-.*-mac-cyrillic" . 0.9) (".*monaco-bold-.*-mac-roman" . 0.9)
		("-cdac$" . 1.3)))
	)
      (set-frame-default)
      ;; コマンドから open -a Emacs.app されたときに新しいフレームを開かない
      (setq ns-pop-up-frames nil)
      
      ;; 最近使ったファイル
      (recentf-mode t)
      (setq recentf-max-menu-items 10)
      (setq recentf-max-saved-items 20)
      ;; (setq recentf-exclude '("^/[^/:]+:")) ;除外するファイル名
      ;; MacPorts のemacs-app はデフォルトでMacのキーバインド使える
      ;; C-zで最小化してうざいので無効に
      (global-unset-key "\C-z")
      ;; Option キーを Meta キーとして使う
      (setq mac-option-modifier 'meta)
      ;; C-x <left/right>でバッファの切り替え（これはデフォルト動作）
      ;; C-x <up/down>でフレームの切り替え
      (global-set-key [?\C-x up] '(lambda () "" (interactive) (other-frame -1)))
      (global-set-key [?\C-x down] '(lambda () "" (interactive) (other-frame 1)))

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

      ;; スクロールゆっくり
      (global-set-key [wheel-up] '(lambda () "" (interactive) (scroll-down 1)))
      (global-set-key [wheel-down] '(lambda () "" (interactive) (scroll-up 1)))
      (global-set-key [double-wheel-up] '(lambda () "" (interactive) (scroll-down 2)))
      (global-set-key [double-wheel-down] '(lambda () "" (interactive) (scroll-up 2)))
      (global-set-key [triple-wheel-up] '(lambda () "" (interactive) (scroll-down 3)))
      (global-set-key [triple-wheel-down] '(lambda () "" (interactive) (scroll-up 3)))

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

      ;; grep のコマンドは find -print0 |xargs grep を使う
;      (require 'grep)
;      (grep-apply-setting 'grep-find-use-xargs 'gnu)
;      (setenv "PATH" (format "%s:%s"
;			     (expand-file-name "~/.rvm/rubies/ruby-2.0.0-p247/bin")
;			     (getenv "PATH")))

))
; X用
(if (eq window-system 'x)
    (progn
      (define-key function-key-map [backspace] [8])
      (put 'backspace 'ascii-character 8)))



;; load-path の追加
(defun add-load-path (path)
  (setq path (expand-file-name path))
  (unless (member path load-path)
    (add-to-list 'load-path path)))

(add-load-path "~/.emacs.d")
(add-load-path "~/.emacs.d/foreign-regexp")
(add-load-path "~/.emacs.d/cscope")
(add-load-path "~/.emacs.d/el-get")

(if (eq window-system 'w32)
    (if (file-accessible-directory-p "c:/cygwin")
	(add-load-path "c:/cygwin/usr/share/emacs/site-lisp")
      (if (file-accessible-directory-p "c:/cygwin64")
	  (add-load-path "c:/cygwin64/usr/share/emacs/site-lisp")))
  (add-load-path "/opt/local/share/emacs/site-lisp"))

(if (eq window-system 'w32)
    (progn
      (setenv "SHELL" "C:/cygwin64/bin/bash.exe")
      (setq shell-file-name "C:/cygwin64/bin/bash.exe")
      (setq explicit-shell-file-name "C:/cygwin64/bin/bash.exe")))


;; el-get パッケージマネージャ
(require 'el-get)
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(ede-project-directories (quote ("/Users/k-shimizu/Desktop/Sample/Src")))
 '(el-get-dir "~/.emacs.d/el-get-packages/")
 '(foreign-regexp/regexp-type 'ruby)
 '(reb-re-syntax 'foreign-regexp)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 '(reb-re-syntax (quote foreign-regexp))
 '(safe-local-variable-values (quote ((ruby-compilation-executable . "ruby") (ruby-compilation-executable . "ruby1.8") (ruby-compilation-executable . "ruby1.9") (ruby-compilation-executable . "rbx") (ruby-compilation-executable . "jruby") (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (whitespace-line-column . 80) (whitespace-style face trailing lines-tail) (require-final-newline . t))))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;(require 'helm-config)
(global-set-key "\M-x" 'helm-mini)


;; リージョンをハイライト
;; C-g で解除(マークは残っているがリージョンは無効)
;; C-x C-x でリージョンを復活
;; M-; ハイライトがあればコメントアウト
(transient-mark-mode 1)

;; ウインドウ移動をShift+矢印で
(windmove-default-keybindings)

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

;; 自動でrevert-buffer
;; ↓こいつをnon-nilにしておくと、vcsによる変更もチェックしてくれる
(setq auto-revert-check-vc-info t)
(setq auto-revert-interval 30)
(add-hook 'find-file-hook
	  '(lambda ()
	     (when (and buffer-file-name
			(vc-backend buffer-file-name))
	       (auto-revert-mode))))

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

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t) ;; MELPAを追加
(package-initialize)

;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (expand-file-name "~/.emacs.d/auto-complete/dict"))
(ac-config-default)

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


;; grep の結果画面は画面端で折り返さないけど、
;; コンパイルの結果画面は画面端で折り返す
(add-hook 'compilation-mode-hook
          '(lambda ()
	     (cond ((eq major-mode 'grep-mode)
		    (setq truncate-lines t)))))

;; Coffee-mode
(autoload 'coffee-mode "coffee-mode" "Major mode for editing Coffeescript." t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(defun coffee-custom ()
  "coffee-mode-hook"
  (setq indent-tabs-mode nil)
  (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook
  '(lambda()
     (coffee-custom)))

;; ruby-mode
(if (eq window-system 'ns) ;;何故かターミナルで動かない そのうち調べる
    (progn
      (require 'rvm)
      (rvm-use-default)))

(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.\\(rb\\|rake\\)$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\.builder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
  interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ruby-insert-encoding-magic-comment nil))
(require 'ruby-mode)
(defun ruby-mode-set-encoding () nil)

;(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
	  '(lambda ()
;	     (message "dbg1:%s\n" ruby-mode-map)
	     (define-key ruby-mode-map "{" nil);ここの設定はEmacs24.3のバグ?
	     (define-key ruby-mode-map "}" nil);
;	     (message "dbg2:%s\n" ruby-mode-map)
;	     (inf-ruby-keys)
	     (make-local-variable 'ac-ignores)
	     (add-to-list 'ac-ignores "end")
	     ))

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
    ;; flymake-haml
    (require 'flymake-easy)
    (require 'flymake-haml)
    (flymake-haml-load)
    (c-set-offset 'substatement-open '0)
    (setq tab-width  8
          indent-tabs-mode nil)))

;; flymake for ruby
(require 'flymake)
;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
	 (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))
(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)
(add-hook
 'ruby-mode-hook
 '(lambda ()
    ;; Don't want flymake mode for ruby regions in rhtml files
    (if (not (null buffer-file-name)) (flymake-mode))))

;; エラー行で C-c d するとエラーの内容をミニバッファで表示する
(defun credmp/flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no (flymake-current-line-no))
	 (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info line-no)))
	 (count (length line-err-info-list))
	 )
    (while (> count 0)
      (when line-err-info-list
	(let* ((file (flymake-ler-file (nth (1- count) line-err-info-list)))
	       (full-file (flymake-ler-full-file (nth (1- count) line-err-info-list)))
	       (text (flymake-ler-text (nth (1- count) line-err-info-list)))
	       (line (flymake-ler-line (nth (1- count) line-err-info-list))))
	  (message "[%s] %s" line text)
	  )
	)
      (setq count (1- count)))))
(add-hook
 'ruby-mode-hook
 '(lambda ()
    (define-key ruby-mode-map "\C-cd" 'flymake-display-err-menu-for-current-line)))


;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs (expand-file-name  "~/.emacs.d/el-get-packages/yasnippet/"))
(yas-global-mode 1)
(yas-load-directory (expand-file-name "~/.emacs.d/yasnippets-rails/rails-snippets/"))
(yas--initialize)

;(message "after-yasnippet")

;; rinari
;; ちなみに閉じタグを出すのは C-c /
;; キーバインド	findするディレクトリ
;; C-c ; f f	RAILS_ROOT/
;; C-c ; f c	app/controller/
;; C-c ; f m	app/models/
;; C-c ; f v	app/views/
;; C-c ; f h	app/helper/
;; C-c ; f i	db/migrate/
;; C-c ; f n	config/
;; C-c ; f e	config/environment/
;; C-c ; f j	pubic/javascript/
;; C-c ; f l	vendor/plugin/
;; C-c ; f o	log/
;; C-c ; f p	public/
;; C-c ; f s	script/
;; C-c ; f t	test/
;; C-c ; f w	lib/workers/
;; C-c ; f x	test/fixtures/
;; C-c ; f y	public/stylesheets/
;; C-c ; f r	spec/
;; C-c ; f z	spec/fixtures
;; C-c ; g      grep
(require 'rinari)
;(message "after-rinari-req")
(require 'rhtml-mode)
;(message "after-rhtml")
(add-hook 'rhtml-mode-hook
	  (lambda () (rinari-launch)))
(add-hook 'haml-mode-hook
	  (lambda ()
	    (rinari-launch)
	    (setq indent-tabs-mode nil)))
(add-hook 'ruby-mode-hook
	  (lambda ()
	    (rinari-launch)))
(message "after-rinari")


(require 'smart-compile)
(define-key ruby-mode-map (kbd "C-c c") 'smart-compile)
(define-key ruby-mode-map (kbd "C-c C-c") (kbd "C-c c C-m"))

;; Scheme-mode
(setq scheme-program-name "gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)


(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-t" 'gtags-find-tag)     ;; 定義にジャンプ
         (local-set-key "\M-r" 'gtags-find-rtag)    ;; 参照を検索
         (local-set-key "\M-s" 'gtags-find-symbol)));; 定義にジャンプ
(global-set-key "\M-e" 'gtags-pop-stack)            ;; スタックを戻る

;; C Scope
;; タグ作成は cscope -bR
(require 'xcscope)
(setq cscope-do-not-update-database t)
(setq cscope-truncate-lines t)

;; c-mod
(add-hook
 'c-mode-hook
 '(lambda ()
    ;; cedit
    (load-library "cedet")
    (global-ede-mode 1)
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
    (gtags-mode 1)
    (setq tab-width 4
          c-basic-offset 4
          indent-tabs-mode 1)))

;; c++-mode
(add-hook
 'c++-mode-hook
 '(lambda ()
    ;; cedit
    (load-library "cedet")
    (global-ede-mode 1)
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
    (gtags-mode 1)
    (gtags-make-complete-list)
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


;; PHP-mode
(autoload 'php-mode "php-mode" "PHP mode" t)
(add-to-list 'auto-mode-alist '("\\.\\(ctp\\|php\\|php5\\|inc\\)$" . php-mode))
(add-hook 'php-mode-hook '(lambda ()
                            (setq php-intelligent-tab nil)
                            (setq intelligent-tab nil)
                            (setq indent-tabs-mode t)
                            (setq c-basic-offset 4)
                            (setq tab-width 4)
			    (make-local-variable 'ac-sources)
			    (setq ac-sources '(ac-source-words-in-same-mode-buffers))
                            ) t)
(defun flymake-php-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
	 (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list "php" (list "-c" local-file))))
(push '(".+\\.php$" flymake-php-init) flymake-allowed-file-name-masks)


;; YAML-mode
(autoload 'yaml-mode "yaml-mode" "YAML mode" t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (rinari-launch)
	     (indent-tabs-mode nil)))
;; css-mode
(add-hook 'css-mode-hook
	  '(lambda ()
	     (setq css-indent-offset 2)
	     (indent-tabs-mode nil)
	     (setq css-indent-offset 2)))

;; scss-mode
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(custom-set-variables '(scss-compile-at-save nil))

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

(if (or (eq window-system 'w32) (eq window-system 'mac) (eq window-system 'ns) (eq window-system 'x))
    (progn
      ;; クライアントを終了するとき終了するかどうかを聞かない
      ;; サーバ起動
      (require 'server)
      (unless (server-running-p)
	(progn
	  (server-start)
	  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "outline")))))

;; foreign-regexp
(require 'foreign-regexp)
 ;; Tell re-builder to use foreign regexp.

;;web-mode
(require 'web-mode)

;;; 適用する拡張子
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))

;;; インデント数
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq indent-tabs-mode nil)
  (setq web-mode-html-offset   2)
  (setq web-mode-css-offset    2)
  (setq web-mode-script-offset 2)
  (setq web-mode-php-offset    2)
  (setq web-mode-java-offset   2)
  (setq web-mode-asp-offset    2))
(add-hook 'web-mode-hook 'web-mode-hook)
