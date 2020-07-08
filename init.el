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

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(mac-auto-operator-composition-mode)

(eval-when-compile (require 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-box-icons-alist (quote company-box-icons-all-the-icons))
 '(global-auto-revert-mode t)
 '(package-selected-packages
   (quote
    (evil python-mode lsp-go go-mode exec-path-from-shell better-shell flycheck-rust ivy-rich counsel ivy rust-mode lsp-mode use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.5))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.15)))))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
)

(use-package counsel
  :ensure t
  :config
  (counsel-mode 1)
  (global-set-key "\C-s" 'swiper)
  )

(use-package ivy-rich
  :ensure t
  :after (ivy)
  :config
  (ivy-rich-mode 1)
  )

(use-package rust-mode
  :ensure t)

(use-package lsp-mode
  :ensure t
  :diminish lsp-mode
  :hook (rust-mode . lsp)
  :commands lsp
)

(use-package python-mode
  :config
  (add-hook 'python-mode-hook #'lsp))


(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  )

(use-package flycheck
  :ensure t
  :hook (after-init-hook . global-flycheck-mode)
  )

(use-package flycheck-rust
  :after (flycheck rust-mode)
  :ensure t
  :hook (flycheck-mode . flycheck-rust-setup)
  )

(use-package company
  :ensure t)

(use-package company-lsp
  :ensure t
  :after company
  :hook (after-init . global-company-mode)
  :init
  (push 'company-lsp company-backends)
)

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode t)
  (global-set-key "\C-z" 'undo)
  (global-set-key (kbd "C-S-z") 'undo-tree-redo)
)

(use-package all-the-icons
  :ensure t)

(use-package neotree
  :ensure t
  :after all-the-icons
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (global-set-key [f8] 'neotree-toggle)
  )

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  )

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode)
  )

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  )

(use-package company-tabnine
  :ensure t
  )

(add-to-list 'company-backends #'company-tabnine)

(use-package doom-themes
  :ensure t
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

(setq make-backup-files nil)
(setq auto-save-default nil)

(set-face-attribute 'default nil
		    :family "Fira Code"
		    :height 140)

(set-fontset-font t
		  'japanese-jisx0208
		  (font-spec :family "Hiragino Sans")
		  )


(global-linum-mode 1)
(setq linum-format "%5s ")

(define-key global-map [?¥] [?\\])

(tool-bar-mode -1)
(setq lsp-rust-server 'rust-analyzer)
