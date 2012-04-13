;; .emacs.el
;; last update 2012/3/28
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
      (setenv "CYGWIN" "nodosfilewarning")
;      (setq find-grep-options " | sed 's/^\\/cygdrive\\/\\([a-z]\\)/\\1:/g'")
      (setq grep-command "grep -n -e ")
      (setq grep-program "grep")
      (setq grep-find-command "find \"z:/share/lcd/lcd_src/20110804_project_utf8/src\" -type f -name \"*.[ch]\" -print0|xargs -0e grep -ne ")
      (defun fg () (interactive)
	(let ((grep-find-command (concat grep-find-command (thing-at-point 'symbol))))
	  (call-interactively 'grep-find)))

      ;; ここまで
      (server-start)
      (remove-hook
       'kill-buffer-query-functions 
       'server-kill-buffer-query-function)))

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
      ;; サーバ起動
      (server-start)
      ;; クライアントを終了するとき終了するかどうかを聞かない
      (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
      
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

;; load-path の追加
(defun add-load-path (path)
  (setq path (expand-file-name path))
  (unless (member path load-path)
    (setq load-path (cons path load-path))))

(add-load-path "~/.emacs.d")
;(add-load-path "~/.emacs.d/emacs-rails")
(add-load-path "~/.emacs.d/rinari")
(add-load-path "~/.emacs.d/haml-mode")
(add-load-path "~/.emacs.d/rhtml")
(add-load-path "~/.emacs.d/coffee-mode/")
(add-load-path "/usr/share/emacs/site-lisp")

;; リージョンをハイライト
;; C-g で解除(マークは残っているがリージョンは無効)
;; C-x C-x でリージョンを復活
;; M-; ハイライトがあればコメントアウト
(transient-mark-mode 1)

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


;画面端でおりかえす
(setq truncate-partial-width-windows nil)

;; 対応する括弧を光らせる
(show-paren-mode t)

;; upcase[C-x C-u] downcase[C-x C-l] を問い合わせなしで実行
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;汎用機の SPF (mule みたいなやつ) には
;;画面を 2 分割したときの 上下を入れ替える swap screen
;;というのが PF 何番かにわりあてられていました。
(defun swap-screen()
  "Swap two screen,leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))
(defun swap-screen-with-cursor()
  "Swap two screen,with cursor in same buffer."
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window 1)
    (set-window-buffer thiswin (window-buffer))
    (set-window-buffer (selected-window) thisbuf)))
;(global-set-key [f2] 'swap-screen)
;(global-set-key [S-f2] 'swap-screen-with-cursor)


;; Don't show Welcome Page
(setq inhibit-startup-message t)
;; Stop beep and flush
(setq ring-bell-function 'ignore)

 ;Show column number
(column-number-mode t)
;Show line number on left.
;; (require 'wb-line-number)
;; (wb-line-number-toggle)

;; ウインドウ分割のundo redo C-x Left, C-x Right
(winner-mode)
(global-set-key (kbd "C-x <left>") 'winner-undo)
(global-set-key (kbd "C-x <right>") 'winner-redo)

;; もとからあるバッファのswitch を退避
(global-set-key "\M-p" 'previous-buffer)
(global-set-key "\M-n" 'next-buffer)


;; 全環境共通 key-bindings
(if (eq window-system 'x)
    (progn
      (define-key function-key-map [backspace] [8])
      (put 'backspace 'ascii-character 8)))
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

(defun revert-buffer-force ()
  (interactive)
  (revert-buffer t t))
(define-key global-map "\C-c\C-x\C-j" 'revert-buffer-force)
(define-key global-map "\C-c\C-x\ j" 'revert-buffer-force)
(define-key global-map "\C-x\C-j" 'revert-buffer)
(define-key global-map "\C-x\ j" 'revert-buffer)

;Elisp Installer
(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/")

;Anything
(require 'anything-config)
(setq anything-sources (list anything-c-source-buffers
                             anything-c-source-bookmarks
                             anything-c-source-recentf
                             anything-c-source-file-name-history
                             anything-c-source-locate))
;(define-key anything-map (kbd "\C-p") 'anything-previous-line)
;(define-key anything-map (kbd "\C-n") 'anything-next-line)
;(define-key anything-map (kbd "\C-v") 'anything-next-source)
;(define-key anything-map (kbd "\M-v") 'anything-previous-source)
(global-set-key "\C-ca" 'anything)

(require 'anything)
(require 'anything-rcodetools)
;; Command to get all RI entries.
;(setq rct-get-all-methods-command "refe -l")
;; See docs
;(define-key anything-map "\C-z" 'anything-execute-persistent-action)


;; (load "emacs256color-hack.el")
;; (require 'color-theme)
;; (color-theme-initialize)
;; (color-theme-calm-forest)
;; (global-font-lock-mode t)

;; CSharp-mode
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
;; (defun my-csharp-mode-fn ()
;;   "function that runs when csharp-mode is initialized for a buffer."
;;   ...insert your code here...
;;   ...most commonly, your custom key bindings ...
;;   )
(add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)

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
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(setq auto-mode-alist
  (append '(("\\.\\(rb\\|rake\\)$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
  interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby" "")
(autoload 'inf-ruby-keys "inf-ruby" "")
(add-hook 'ruby-mode-hook
          '(lambda ()
            (inf-ruby-keys)))

;; HAML
;; C-i でインデント C-I でアンインデント
(autoload 'haml-mode "haml-mode" "Mode for editing HAML" t)
(setq auto-mode-alist
      (append '(("\\.haml$" . haml-mode)) auto-mode-alist))
(add-hook
 'haml-mode-hook
 '(lambda ()
    (c-set-offset 'substatement-open '0)
    (setq tab-width  8
          indent-tabs-mode nil)))



;; flymake のエラーをミニバッファに出す
(defun credmp/flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list))
         )
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)
          )
        )
      (setq count (1- count)))))
(add-hook
 'ruby-mode-hook
 '(lambda ()
    (define-key ruby-mode-map "\C-cd" 'flymake-display-err-menu-for-current-line)))




;; ;; rails
;; ;; rails.el
;; (defun try-complete-abbrev (old)
;;   (if (expand-abbrev) t nil))
;; (setq hippie-expand-try-functions-list
;;       '(try-complete-abbrev
;;         try-complete-file-name
;;         try-expand-dabbrev))
;; (setq rails-use-mongrel t)
;; (require 'rails)
;; ;; 対応するファイルへの切り替え(C-c C-p)
;; (define-key rails-minor-mode-map "\C-c\C-p" 'rails-lib:run-primary-switch)
;; ;; 行き先を選べるファイル切り替え(C-c C-n)
;; (define-key rails-minor-mode-map "\C-c\C-n" 'rails-lib:run-secondary-switch)
;; (setq auto-mode-alist  (cons '("\\.rhtml$" . html-mode) auto-mode-alist))

;; Rails -> rinari
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
;(require 'ido)
;(ido-mode t)
(require 'rinari)
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
	  (lambda () (rinari-launch)))
(add-hook 'haml-mode-hook
	  (lambda ()
	    (rinari-launch)
	    (setq indent-tabs-mode nil)))




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

;; c-mod
(add-hook
 'c-mode-hook
 '(lambda ()
    (gtags-mode 1)
    (gtags-make-complete-list)
    (setq tab-width 8
          c-basic-offset 4
          indent-tabs-mode 1)))

;; c++-mode
(add-hook
 'c++-mode-hook
 '(lambda ()
    (c-set-offset 'substatement-open '0)
    (setq tab-width  8
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
(setq auto-mode-alist
      (cons '("\\.\\(ctp\\|php\\|php5\\|inc\\)$" . php-mode) auto-mode-alist))
(add-hook 'php-mode-hook '(lambda ()
                            (setq php-intelligent-tab nil)
                            (setq intelligent-tab nil)
                            (setq indent-tabs-mode t)
                            (setq c-basic-offset 4)
                            (setq tab-width 4)
                            ) t)

;; YAML-mode
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (indent-tabs-mode nil)))

;; refe
;; (require 'refe)
