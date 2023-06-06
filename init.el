;; init.el is auto-generated from README.org

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package magit)
(use-package yaml-mode)

(use-package auto-package-update
  :custom
  (auto-package-update-prompt-before-update t)
  (auto-package-update-show-preview t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(set-frame-parameter (selected-frame) 'alpha '(95 . 100))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook eshell-mode-hook treemacs-mode-hook))
  (add-hook mode(lambda() (display-line-numbers-mode 0))))

(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(use-package all-the-icons
  :custom (all-the-icons-install-fonts 1))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(set-face-attribute 'default nil :font "JetBrains Mono" :weight 'light :height 180)
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'light :height 190)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'light :height 1.3)

;; Load org-faces to make sure we can set appropriate faces
(require 'org-faces)

;; Hide emphasis markers on formatted text
(setq org-hide-emphasis-markers t)

;; Resize Org headings
;; (dolist (face '((org-level-1 . 1.1)
;;                 (org-level-2 . 1.08)
;;                 (org-level-3 . 1.05)
;;                 (org-level-4 . 1.0)
;;                 (org-level-5 . 1.1)
;;                 (org-level-6 . 1.1)
;;                 (org-level-7 . 1.1)
;;                 (org-level-8 . 1.1)))
;;   (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face)))

;; Make the document title a bit bigger
;; (set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.3)

;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(defun my/org-present-start ()
  ;; Tweak font sizes
  (setq-local face-remapping-alist '(
  (default (:height 1.5) variable-pitch)
  (header-line (:height 2.0) variable-pitch)
  (org-document-title (:height 1.75) org-document-title)
  (org-code (:height 1.5) org-code)
  (org-verbatim (:height 1.5) org-verbatim)
  (org-block (:height 1.25) org-block)
  (org-block-begin-line (:height 0.7) org-block)))

  ;; Center the presentation and wrap lines
  (visual-fill-column-mode 1)
  (visual-line-mode 1)

  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " "))


(defun my/org-present-end ()
  ;; Reset font customizations
  (setq-local face-remapping-alist '((default default default)))

  ;; Stop centering the document
  (visual-fill-column-mode 0)
  (visual-line-mode 0)

  ;; Clear the header line format by setting to `nil'
  (setq header-line-format nil))

;; Turn on variable pitch fonts in Org Mode buffers
(add-hook 'org-mode-hook 'variable-pitch-mode)

(use-package visual-fill-column
  :custom
  (visual-fill-column-width 110)
  (visual-fill-column-center-text t)
  :hook
  (org-present-mode . my/org-present-start)
  (org-present-mode-quit . my/org-present-end))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org
  :custom
  (org-confirm-babel-evaluate nil)
  (org-babel-python-command "python3")
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)))
  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(use-package ob-async
  :custom
  ob-async-no-async-languages-alist '("ipython"))

(use-package org-tanglesync)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-env "JAVA_HOME")
    (exec-path-from-shell-initialize)))

(use-package load-bash-alias
  :config
  (setq load-bash-alias-bashrc-file "~/.bashrc")
  (load-bash-alias-load-bash-aliases-into-eshell)
  (with-current-buffer "*eshell*"
    (insert "clear 1")
    (eshell-send-input)))

(defun efs/tangle-config()
  (let ((thisDirectory (file-name-directory(buffer-file-name)))
	(emacsDirectory (expand-file-name user-emacs-directory)))
    (when (string-equal thisDirectory emacsDirectory)
  (let ((org-confirm-babel-evaluate nil))
	(org-babel-tangle)))))

(add-hook 'org-mode-hook (lambda() (add-hook 'after-save-hook #'efs/tangle-config)))

(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  :custom
  which-key-idle-delay 1.5)

(use-package ivy)
(use-package projectile
  :config (projectile-mode +1)
  :bind (:map global-map ("C-c p" . projectile-command-map))
  :custom ((projectile-completion-system 'ivy)))

(use-package iedit)
(use-package rg
  :config
  (rg-enable-default-bindings))

(use-package lsp-mode)
(use-package lsp-ui)

(use-package company
  :bind
  (:map company-active-map
	("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package lsp-treemacs
  :bind (:map global-map ("C-`" . treemacs)))

(add-to-list 'image-types 'svg)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(autoload 'google-set-c-style "google-c-style")
(autoload 'google-make-newline-indent "google-c-style")
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))

(add-hook 'json-mode-hook
      (lambda ()
        (make-local-variable 'js-indent-level)
        (setq js-indent-level 2)))

(use-package json-mode)

(use-package kotlin-mode)

(use-package dockerfile-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
