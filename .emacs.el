;; .emacs.el
;; last update 2011/7/5
;;
;;
;; .emacs.el を再読み込みするには
;; C-x C-s または Command + s
;; M-x load-file RET ~/.emacs.el RET

;; Emacs
;; GUIの設定が後から動くとなんかうざい感じになるので先に動かす
(if (eq window-system 'mac)
    (progn
      ;; フレームのディフォルトの設定。
      (setq default-frame-alist
            (append (list 
		     '(foreground-color . "white")
		     '(background-color . "black")
		     '(border-color . "black")
		     '(mouse-color . "red")    ; ???
		     '(cursor-color . "white") ;
		     '(width . 120)     ; フレームの横幅
		     '(height . 50)    ; フレームの高さ
;;                     '(top . 100)        ; フレームのX座標の位置
;;                     '(left . 100)       ; フレームのY座標の位置
		     '(alpha . 85)
		     )default-frame-alist))
      ;; サーバ起動
      (server-start)
      ;; クライアントを終了するとき終了するかどうかを聞かない
      (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
      
      (require 'carbon-font)
      ; Hide menu bar and tool bar
      (setq menu-bar-mode nil)
      (tool-bar-mode nil)

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

      (defface hlline-face
	'((((class color)
	    (background dark))
	   ;;(:background "dark state gray"))
	   (:background "gray10"
			:underline "gray24"))
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

;; リージョンをハイライト
;; C-g で解除(マークは残っているがリージョンは無効)
;; C-x C-x でリージョンを復活
;; M-; ハイライトがあればコメントアウト
(transient-mark-mode 1)

;; load-path の追加
(defun add-load-path (path)
  (setq path (expand-file-name path))
  (unless (member path load-path)
    (setq load-path (cons path load-path))))

(add-load-path "~/.emacs.d")
;(add-load-path "~/.emacs.d/emacs-rails")
(add-load-path "~/.emacs.d/rinari")
(add-load-path "~/.emacs.d/rhtml")
(add-load-path "/usr/share/emacs/site-lisp")

;; バックアップ関係
(setq backup-directory-alist `(("." . "/Users/shimizu/.backups")))
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


;key-bindings
(if (eq window-system 'x)
    (progn
      (define-key function-key-map [backspace] [8])
      (put 'backspace 'ascii-character 8)))
;; (global-set-key "\C-h" 'backward-delete-char)
;; (global-set-key "\177" 'delete-char)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-h" 'delete-backward-char)
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

;; ruby-mode
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(setq auto-mode-alist
  (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
  interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby" "")
(autoload 'inf-ruby-keys "inf-ruby" "")
(add-hook 'ruby-mode-hook
          '(lambda ()
            (inf-ruby-keys)))

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

;(require 'ido)
;(ido-mode t)
(require 'rinari)
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
    (lambda () (rinari-launch)))




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
      (cons '("\\.\\(php\\|php5\\|inc\\)$" . php-mode) auto-mode-alist))
(add-hook 'php-mode-hook '(lambda ()
                            (setq php-intelligent-tab nil)
                            (setq intelligent-tab nil)
                            (setq indent-tabs-mode t)
                            (setq c-basic-offset 4)
                            (setq tab-width 4)
                            ) t)

;; refe
;; (require 'refe)
