; Emacs設定ファイル
;; ---
;; Copyright (c) 2020 Hikaru Terazono. All rights reserved.

;; MELPAの設定
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
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
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; 合字の設定（macOSのみ）
(when (memq window-system '(mac))
  (mac-auto-operator-composition-mode)
)

;; Emacs 26.1でのバグ回避．MELPAが正常にうごかないっぽい．
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(eval-when-compile (require 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-box-icons-alist 'company-box-icons-all-the-icons)
 '(global-auto-revert-mode t)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(package-selected-packages
   '(evil python-mode lsp-go go-mode exec-path-from-shell better-shell flycheck-rust ivy-rich counsel ivy rust-mode lsp-mode use-package))
 '(typescript-indent-level 2))
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
  :config
  (ivy-mode 1)
)

(use-package exec-path-from-shell
  :straight t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
  )
)

(use-package counsel
  :straight t
  :config
  (counsel-mode 1)
  (global-set-key "\C-s" 'swiper)
  )

(use-package ivy-rich
  :straight t
  :after (ivy)
  :config
  (ivy-rich-mode 1)
  )

(use-package lsp-mode
  :straight t
  :diminish lsp-mode
  :hook ((rust-mode . lsp) (c++-mode . lsp))
  :commands lsp
  :config
  (setq lsp-enable-semantic-highlighting t)
  (setq lsp-enable-indentation t)
)

(use-package projectile
  :straight t
  :custom
  (projectile-indexing-method 'alien)
  (projectile-use-git-grep 1)
  )

(use-package rust-mode
  :straight t
  :after (lsp-mode)
  :init
  (setq lsp-rust-server 'rust-analyzer)
  :bind
  ("H-i" . lsp-format-buffer)
)

(use-package python-mode
  :straight t
  :config
  (add-hook 'python-mode-hook #'lsp))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  )

(use-package flycheck
  :straight t
  :hook (after-init-hook . global-flycheck-mode)
  )

(use-package flycheck-rust
  :after (flycheck rust-mode)
  :straight t
  :hook (flycheck-mode . flycheck-rust-setup)
  )

(use-package company
  :straight t
  :init
  (with-eval-after-load "company"
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)
    )
)

(use-package undo-tree
  :straight t
  :init
  (global-undo-tree-mode t)
  (global-set-key "\C-z" 'undo)
  (global-set-key (kbd "C-S-z") 'undo-tree-redo)
)

(use-package all-the-icons
  :straight t)

(use-package neotree
  :straight t
  :after (all-the-icons projectile)
  :custom
  (neo-theme 'nerd2)
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-window-fixed-size nil)
)

(use-package yasnippet
  :straight t
  :init
  (yas-global-mode 1)
)

(use-package which-key
  :straight t
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  )

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode)
  )

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  )

(use-package doom-themes
  :straight t
  :init
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

(use-package vue-mode
  :straight t
  :after (lsp-mode flycheck)
  :config
  (setq mmm-submode-decoration-level 0)
  (flycheck-add-mode 'javascript-eslint 'vue-mode)
  (flycheck-add-mode 'javascript-eslint 'css-mode)
  :hook (
  (vue-mode . lsp)
  )
)

(use-package typescript-mode
  :straight t
  :after (lsp-mode flycheck)
  :config
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  :hook (
  (typescript-mode . lsp)
  )
)

(use-package add-node-modules-path
  :straight t
  :after (typescript-mode vue-mode)
  :hook
  (
   (js-mode . add-node-modules-path)
   (typescript-mode . add-node-modules-path)
   (vue-mode . add-node-modules-path)
  )
)

;;(use-package tree-sitter
;;  :straight t
;;  :config
;;  (global-tree-sitter-mode)
;;  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
;;  )

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook))

(use-package cmake-mode
  :straight t
  :config
  (setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist)))

(use-package centaur-tabs
  :straight t
  :init
  (centaur-tabs-mode t)
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-bar 'under)
  (setq x-underline-at-descent-line t)
  (centaur-tabs-change-fonts "Helvetica" 120)
  :bind
  ("M-<left>"  . centaur-tabs-backward)
  ("M-<right>" . centaur-tabs-forward)
  ("H-t"       . centaur-tabs-mode)
  ("H-w"       . kill-this-buffer)
  )

(use-package magit
  :straight t)

(use-package git-gutter-fringe
  :straight t
  :init
  (global-git-gutter-mode t)
  :config
  (fringe-helper-define 'git-gutter-fr:modified nil
  "X......."
  "X......."
  "X......."
  "X......."
  "X......."
  "X......."
  "X......."
  "X.......")
  (fringe-helper-define 'git-gutter-fr:deleted nil
  "XXXXXXXX"
  "XXXXXXXX"
  "........"
  "........"
  "........"
  "........"
  "........"
  "........")
  (fringe-helper-define 'git-gutter-fr:added nil
  "XX......"
  "XX......"
  "XX......"
  "XX......"
  "XX......"
  "XX......"
  "XX......"
  "XX......")
)

(setq-default indent-tabs-mode nil)
(setq-default electric-indent-inhibit t)

;; バックアップファイル類は無効にする
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; フォントとか．
(set-face-attribute 'default nil
		    :family "Fira Code"
		    :height 130)

(set-face-attribute 'neo-file-link-face   nil :family "Helvetica")
(set-face-attribute 'neo-dir-link-face    nil :family "Helvetica")
(set-face-attribute 'neo-header-face      nil :family "Helvetica")
(set-face-attribute 'neo-expand-btn-face  nil :family "Helvetica")

(set-fontset-font t
		  'japanese-jisx0208
		  (font-spec :family "Hiragino Sans")
		  )

;; 行数表示を良い感じに．
(global-linum-mode 1)
(setq linum-format "%5s ")

;; ATOK使うのがいけないんだよ...
(define-key global-map [?¥] [?\\])

(tool-bar-mode -1)
(when (eq window-system 'ns)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; ほかのGUIアプリっぽいペースト（yank）の挙動にする
(delete-selection-mode t)

;; macOSスタイルのキーバインドを指定（Hyperキー）
(when (memq window-system '(mac ns))
  (global-set-key [(hyper q)] 'save-buffers-kill-terminal)
  (global-set-key [(hyper n)] 'make-frame-command)
  (global-set-key [(hyper a)] 'mark-whole-buffer)
  (global-set-key [(hyper v)] 'yank)
  (global-set-key [(hyper c)] 'kill-ring-save)
  (global-set-key [(hyper x)] 'kill-region)
  (global-set-key [(hyper s)] 'save-buffer)
  (global-set-key [(hyper l)] 'goto-line)
  (global-set-key [(hyper z)] 'undo)
  (global-set-key [(hyper f)] 'swiper)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'hyper)
  )

(when (window-system)
  (setq frame-title-format
      (if (buffer-file-name)
          (format "%%f - Emacs")
        (format "%%b - Emacs")))
)

;; お察しの通り...
(setq fancy-splash-image (expand-file-name "~/.config/dotfiles/touka.png"))
(setq dashboard-startup-banner (expand-file-name "~/.config/dotfiles/touka.png"))

(defun fish (buffer-name)
  "Start a terminal and rename buffer."
  (interactive "sbuffer name: ")
  (ansi-term "/opt/homebrew/bin/fish")
  (rename-buffer buffer-name t))
