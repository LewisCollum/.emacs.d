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

(use-package auto-package-update
  :custom
  (auto-package-update-prompt-before-update t)
  (auto-package-update-show-preview t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

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

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)))
  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(use-package ob-async
  :custom
  ob-async-no-async-languages-alist '("ipython"))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
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

(defun mkcd(directory)
  (let ((makeParentDirectories t))
    (make-directory directory makeParentDirectories)
    (cd directory)))

(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  :custom
  which-key-idle-delay 1.5)

(use-package ivy)
(use-package projectile
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
  :bind
  (:map global-map
	("C-<tab>" . treemacs)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(gradle-mode which-key use-package projectile org-bullets ob-async magit lsp-ui lsp-treemacs load-bash-alias ivy exec-path-from-shell doom-themes doom-modeline company-box auto-package-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
