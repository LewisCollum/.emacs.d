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
(dolist (mode '(org-mode-hook
		eshell-mode-hook))
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
  (load-bash-alias-load-bash-aliases-into-eshell))
