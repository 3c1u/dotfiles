;; Emacs設定ファイル
;; ---
;; Copyright (c) 2020 Hikaru Terazono. All rights reserved.

;; MELPAの設定
(require 'package)
(let* ((no-ssl (and (memq system-type
			  '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives
	       (cons "melpa" (concat proto "://melpa.org/packages/"))
	       t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives
		 (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
					user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer (url-retrieve-synchronously "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
						     'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; 合字の設定（macOSのみ）
(when (memq window-system
	    '(mac))
  (mac-auto-operator-composition-mode))

;; Emacs 26.1でのバグ回避．MELPAが正常にうごかないっぽい．
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(eval-when-compile (require 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-box-icons-alist (quote company-box-icons-all-the-icons))
 '(global-auto-revert-mode t)
 '(package-selected-packages (quote (evil python-mode lsp-go go-mode exec-path-from-shell
					  better-shell flycheck-rust ivy-rich counsel
					  ivy rust-mode lsp-mode use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.5))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.15)))))

(use-package ivy
  :straight t
  :config (ivy-mode 1))

(use-package exec-path-from-shell
  :straight t
  :init (when (memq window-system
		    '(mac ns x))
	  (exec-path-from-shell-initialize)))

(use-package counsel
  :straight t
  :config (counsel-mode 1)(global-set-key "\C-s" 'swiper))

(use-package ivy-rich
  :straight t
  :after (ivy):config
  (ivy-rich-mode 1))

(use-package lsp-mode
  :straight t
  :diminish lsp-mode
  :hook ((rust-mode . lsp)
	 (csharp-mode . lsp)
	 (lsp-mode . lsp-ui-mode)
	 (lsp-managed-mode . (lambda ()
			       (setq-local company-backends
					   '(company-capf))))):commands
  lsp
  :config (setq lsp-csharp-server-path "~/.local/bin/omnisharp"))

(use-package rust-mode
  :straight t
  :after (lsp-mode):init
  (setq lsp-rust-server 'rust-analyzer))

;; Xcode標準のSourceKit-LSPを使うので，Linuxだとパスが違う？
(use-package lsp-sourcekit
  :after lsp-mode
  :straight t
  :config (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

(use-package swift-mode
  :straight t
  :hook (swift-mode . (lambda ()
			(lsp))))

(use-package python-mode
  :straight t
  :config (add-hook 'python-mode-hook #'lsp))


(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode))

(use-package flycheck
  :straight t
  :hook (after-init-hook . global-flycheck-mode))

(use-package flycheck-rust
  :after (flycheck rust-mode):straight
  t
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package company :straight t)

;; (use-package company-lsp
;;   :straight t
;;   :after company
;;   :hook (after-init . global-company-mode)
;;   :init
;;   (push 'company-lsp company-backends)
;; )

(use-package undo-tree
  :straight t
  :init (global-undo-tree-mode t)(global-set-key "\C-z" 'undo)(global-set-key (kbd "C-S-z")
									      'undo-tree-redo))

(use-package all-the-icons :straight t)

(use-package neotree
  :straight t
  :after all-the-icons
  :init (setq neo-theme (if (display-graphic-p)
			    'icons
			  'arrow))(global-set-key [f8]
						  'neotree-toggle))

(use-package yasnippet
  :straight t
  :init (yas-global-mode 1))

(use-package which-key
  :straight t
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode))

(use-package csharp-mode :straight t)

;; (use-package omnisharp
;;   :straight t
;;   :after (company flycheck csharp-mode)
;;   :init (add-to-list'company-backends 'company-omnisharp)
;;   (add-hook 'csharp-mode-hook #'company-mode)
;;   (add-hook 'csharp-mode-hook #'flycheck-mode)
;;   (setq omnisharp-server-executable-path "/usr/local/bin/omnisharp")
;; )

;; 便利だけどやっぱ重い．焼ける．
;; (use-package company-tabnine
;;  :straight t
;;  :init
;;  (add-to-list 'company-backends #'company-tabnine)
;;  )

(use-package doom-themes :straight t
  :init
  ;; Global settings (defaults) (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled doom-themes-enable-italic
  t)(load-theme 'doom-one t)(doom-themes-visual-bell-config)(doom-themes-neotree-config)(doom-themes-org-config))

(use-package ligature :load-path "~/.config/dotfiles/ligature.el"
  :config
  ;; Enable the www ligature in every possible major mode (ligature-set-ligatures 't '("www"))(ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::" ":::"
  ":=" "!!" "!=" "!==" "-}" "----" "-->" "->"
  "->>" "-<" "-<<" "-~" "#{" "#[" "##" "###"
  "####" "#(" "#?" "#_" "#_(" ".-" ".=" ".."
  "..<" "..." "?=" "??" ";;" "/*" "/**" "/="
  "/==" "/>" "//" "///" "&&" "||" "||=" "|="
  "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
  "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-"
  ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>"
  "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->"
  "<+" "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<"
  "<<-" "<<=" "<<<" "<~" "<~~" "</" "</>" "~@"
  "~-" "~>" "~~" "~~>" "%%"))(global-ligature-mode 't))

(use-package srefactor
  :straight t
  :init (require 'srefactor-lisp))

;; バックアップファイル類は無効にする
(setq make-backup-files nil)
(setq auto-save-default nil)

;; フォントとか．
(set-face-attribute 'default nil :family "Fira Code"
		    :height 140)

(set-fontset-font t
		  'japanese-jisx0208
		  (font-spec :family "Hiragino Sans"))

;; 行数表示を良い感じに．
(global-linum-mode 1)
(setq linum-format "%5s ")

;; ATOK使うのがいけないんだよ...
(define-key global-map [?¥] [?\\])

(tool-bar-mode -1)

;; ほかのGUIアプリっぽいペースト（yank）の挙動にする
(delete-selection-mode t)

;; macOSスタイルのキーバインドを指定（Hyperキー）
(when (memq window-system
	    '(mac ns))
  (global-set-key [(hyper q)]
		  'save-buffers-kill-terminal)
  (global-set-key [(hyper n)]
		  'make-frame-command)
  (global-set-key [(hyper a)]
		  'mark-whole-buffer)
  (global-set-key [(hyper v)]
		  'yank)
  (global-set-key [(hyper c)]
		  'kill-ring-save)
  (global-set-key [(hyper s)]
		  'save-buffer)
  (global-set-key [(hyper l)]
		  'goto-line)
  (global-set-key [(hyper w)]
		  (lambda ()
		    (interactive)
		    (delete-window)))
  (global-set-key [(hyper z)]
		  'undo)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'hyper))

;; お察しの通り...
(setq fancy-splash-image (expand-file-name "~/.config/dotfiles/touka.png"))

(defun fish (buffer-name)
  "Start a terminal and rename buffer."
  (interactive "sbuffer name: ")
  (term "/usr/local/bin/fish")
  (rename-buffer buffer-name t))
