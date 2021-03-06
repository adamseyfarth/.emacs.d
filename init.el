;;; init.el --- Adam Seyfarth's Emacs configuration

;; Author: Adam Seyfarth <adam@seyfarth.name>

;;; Commentary:

;; Should be in the repo https://github.com/adamseyfarth/.emacs.d
;; Uses use-package to make sure elpa packages are installed.

;;; Code:

(defun lisp-interaction-mode-keys ()
  "Repair keymap for `lisp-interaction-mode'."
  ; Paredit takes over C-j, which is a useful command
  (local-set-key (kbd "C-S-j") 'eval-print-last-sexp))

(add-hook 'find-file-hook 'linum-mode t)
(add-hook 'find-file-hook 'delete-selection-mode 1)
(add-hook 'prog-mode-hook 'flyspell-prog-mode t)
(add-hook 'lisp-interaction-mode-hook 'lisp-interaction-mode-keys)
(setq-default cursor-type 'bar)
(require 'server)
(when (and window-system (not (server-running-p))) (server-start))
(setq inhibit-startup-screen t)

(require 'python)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args ""
      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
      python-shell-completion-setup-code
      "from IPython.core.completerlib import module_completion"
      python-shell-completion-module-string-code
      "';'.join(module_completion('''%s'''))\n"
      python-shell-completion-string-code
      "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
; (package-refresh-contents)
(unless (package-installed-p 'use-package) (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Power emacs
(use-package auto-complete
  :init (add-hook 'find-file-hook 'auto-complete-mode t))
(use-package expand-region
  :bind ("C-=" . er/expand-region))
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)
	 ("C-S-<mouse-1>" . mc/add-cursor-on-click)))
(use-package undo-tree
  :init (global-undo-tree-mode))
(use-package projectile
  :init (projectile-global-mode))
(use-package flycheck
  :init (add-hook 'after-init-hook #'global-flycheck-mode))
(use-package ag)
;; Git
;; Magit requires emacs version >= 24.4
(when (and (>= emacs-major-version 24)
	   (>= emacs-minor-version 4))
  (use-package magit))
(use-package gitignore-mode)
(use-package gitconfig-mode)
(use-package git-timemachine)
;; Elisp
(use-package s)
(use-package dash)
(use-package dash-functional)
(use-package f)
;; Lisps
(use-package paredit
  :init (dolist (hook '(scheme-mode-hook
			emacs-lisp-mode-hook
			lisp-mode-hook
			lisp-interaction-mode-hook
			clojure-mode-hook
			cider-repl-mode-hook
			hy-mode-hook))
	  (add-hook hook 'paredit-mode t)))
(use-package cider)
(use-package clojure-mode)
(use-package slime)
;; Haskell
(use-package haskell-mode
  :init (add-hook 'haskell-mode-hook 'haskell-indentation-mode))
;; Markup
(use-package typo)
(use-package org)
(use-package markdown-mode
  :init (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))
;; Appearance
(use-package base16-theme
  :init (load-theme 'base16-ashes-dark t))
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode t))
(use-package fill-column-indicator
  :init (setq fci-rule-column 80))
(use-package whitespace
  :init
  (setq whitespace-style '(face trailing))
  (add-hook 'prog-mode-hook 'whitespace-mode t))

; Maybe I'll be crazy enough to use this one day...
;; (use-package ergoemacs-mode
;;   :init
;;   (setq ergoemacs-theme nil)
;;   (setq ergoemacs-keyboard-layout "colemak")
;;   (ergoemacs-mode 1))

(setq org-export-html-style-include-scripts nil
      org-export-html-style-include-defaultgs nil
      org-export-html-style
      "<link rel=\"stylesheet\" type=\"text/css\" href=\"org-style.css\" />")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes (quote ("d72836155cd3b3e52fd86a9164120d597cbe12a67609ab90effa54710b2ac53b" "73ae6088787f6f72ef52f19698b25bc6f0edf47b9e677bf0a85e3a1e8a7a3b17" "cda6cb17953b3780294fa6688b3fe0d3d12c1ef019456333e3d5af01d4d6c054" "6ebb2401451dc6d01cd761eef8fe24812a57793c5ccc427b600893fa1d767b1d" "f0e69da2cf73c7f153fc09ed3e0ba6e1fd670fec09b8a6a8ed7b4f9efea3b501" "0240d45644b370b0518e8407f5990a243c769fb0150a7e74297e6f7052a04a72" "6dbd0dd4c344f1ca534422cc5a1fd3ed822dcde947ae983948b70c7284a0ed33" "3f04f37604c5f5cc3c71bc1a4a604ed8be340d0f150946b25658e403ccbad6d2" "a88946e2135d3635daf6789c475c7f093e4471ded5391253d6f88bd5271ee4be" "30611406f83fae3d001e917b03ad47bbd1c7797cf640a2e7db9d2445741e2554" "92d131a9c3ffa953b1265e9fec50d0bd366c0481305119b851055afeb4130686" "ff5acbbf20c7ba4889eb2b14395fcd55eeecbfb57853e47c7d514503ad83d6bb" "00f09a2728377a37e9a24d631de94cc7440e0803e218474cac287061951c205c" "7545d3bb77926908aadbd525dcb70256558ba05d7c478db6386bfb37fb6c9120" "9f3a4edb56d094366afed2a9ba3311bbced0f32ca44a47a765d8ef4ce5b8e4ea" default)))
 '(display-time-mode t)
 '(font-use-system-font nil)
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(indicate-buffer-boundaries (quote left))
 '(org-agenda-files (quote ("~/plan/plan.org")))
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fixed" :foundry "Misc" :slant normal :weight normal :height 120 :width normal)))))

;;; init.el ends here
