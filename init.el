;;; init.el --- Adam Seyfarth's Emacs configuration

;; Author: Adam Seyfarth <adam@seyfarth.name>

;;; Commentary:

;; Should be in the repo https://github.com/adamseyfarth/.emacs.d
;; Uses use-package to make sure elpa packages are installed.

;;; Code:

(add-hook 'find-file-hook 'linum-mode t)
(add-hook 'find-file-hook 'delete-selection-mode 1)
(add-hook 'prog-mode-hook 'flyspell-prog-mode t)
(setq-default cursor-type 'bar)
(require 'server)
(when (and window-system (not (server-running-p))) (server-start))
(setq inhibit-startup-screen t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)
(unless (package-installed-p 'use-package) (package-install 'use-package))
(require 'use-package)

;; Power emacs
(use-package auto-complete :ensure t
  :init (add-hook 'find-file-hook 'auto-complete-mode t))
(use-package expand-region :ensure t
  :bind ("C-=" . er/expand-region))
(use-package multiple-cursors :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)
	 ("C-S-<mouse-1>" . mc/add-cursor-on-click)))
(use-package undo-tree :ensure t
  :init (global-undo-tree-mode))
(use-package projectile :ensure t
  :init (projectile-global-mode))
(use-package flycheck :ensure t
  :init (add-hook 'after-init-hook #'global-flycheck-mode))
(use-package ag :ensure t)
;; Git
;; Magit (and its git-commit) require emacs version >= 24.4
(let* ((version-strs (split-string emacs-version "\\."))
       (version-nums (mapcar 'string-to-number version-strs))
       (major-version (nth 0 version-nums))
       (minor-version (nth 1 version-nums)))
  (when (and (>= major-version 24)
	     (>= minor-version 4))
    (use-package git-commit :ensure t)
    (use-package magit :ensure t)))
(use-package gitignore-mode :ensure t)
(use-package gitconfig-mode :ensure t)
(use-package git-timemachine :ensure t)
;; Elisp
(use-package s :ensure t)
(use-package dash :ensure t)
(use-package dash-functional :ensure t)
(use-package f :ensure t)
;; Lisps
(use-package paredit :ensure t
  :init (dolist (hook '(scheme-mode-hook
			emacs-lisp-mode-hook
			lisp-mode-hook
			lisp-interaction-mode-hook
			clojure-mode-hook
			cider-repl-mode-hook
			hy-mode-hook))
	  (add-hook hook 'paredit-mode t)))
(use-package cider :ensure t)
(use-package clojure-mode :ensure t)
(use-package slime :ensure t)
;; Haskell
(use-package haskell-mode :ensure t
  :init (add-hook 'haskell-mode-hook 'haskell-indentation-mode))
;; Markup
(use-package typo :ensure t)
(use-package org :ensure t)
(use-package markdown-mode :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))
;; Appearance
(use-package base16-theme :ensure t
  :init (load-theme 'base16-default-dark t))
(use-package rainbow-delimiters :ensure t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode t))
(use-package fill-column-indicator :ensure t
  :init (setq fci-rule-column 80))
(use-package whitespace :ensure t
  :init (setq whitespace-style '(face trailing)))

; Maybe I'll be crazy enough to use this one day...
;; (use-package ergoemacs-mode :ensure t
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
 '(custom-safe-themes (quote ("a88946e2135d3635daf6789c475c7f093e4471ded5391253d6f88bd5271ee4be" "30611406f83fae3d001e917b03ad47bbd1c7797cf640a2e7db9d2445741e2554" "92d131a9c3ffa953b1265e9fec50d0bd366c0481305119b851055afeb4130686" "ff5acbbf20c7ba4889eb2b14395fcd55eeecbfb57853e47c7d514503ad83d6bb" "00f09a2728377a37e9a24d631de94cc7440e0803e218474cac287061951c205c" "7545d3bb77926908aadbd525dcb70256558ba05d7c478db6386bfb37fb6c9120" "9f3a4edb56d094366afed2a9ba3311bbced0f32ca44a47a765d8ef4ce5b8e4ea" default)))
 '(display-time-mode t)
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(indicate-buffer-boundaries (quote left))
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
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 90 :width normal)))))

(provide 'init)
;;; init.el ends here
