(add-hook 'find-file-hook 'linum-mode t)
(add-hook 'find-file-hook 'delete-selection-mode 1)
(setq-default cursor-type 'bar)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package s :ensure t)
(use-package base16-theme :ensure t
  :init (load-theme 'base16-default-dark t))
(use-package paredit :ensure t
  :init
  (add-hook 'scheme-mode 'paredit-mode t)
  (add-hook 'emacs-lisp-mode 'paredit-mode t)
  (add-hook 'lisp-mode 'paredit-mode t)
  (add-hook 'lisp-interaction-mode 'paredit-mode t)
  (add-hook 'clojure-mode 'paredit-mode t)
  (add-hook 'hy-mode 'paredit-mode t))
(use-package rainbow-delimiters :ensure t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode t))
(use-package cider :ensure t)
(use-package auto-complete :ensure t
  :init (add-hook 'find-file-hook 'auto-complete-mode t))
; Magit requires emacs version >= 24.4,
; but Trisquel has 24.3 in the repos.
(let* ((version-strs (split-string emacs-version "\\."))
       (version-nums (mapcar 'string-to-number version-strs))
       (major-version (nth 0 version-nums))
       (minor-version (nth 1 version-nums)))
  (if (and (>= major-version 24)
	   (>= minor-version 4))
    (use-package magit
      :ensure t)))
(use-package expand-region :ensure t
  :bind ("C-=" . er/expand-region))
(use-package multiple-cursors :ensure t)
(use-package undo-tree :ensure t)
(use-package slime :ensure t)

; Maybe I'll be crazy enough to use this one day...
;; (use-package ergoemacs-mode :ensure t
;;   :init
;;   (setq ergoemacs-theme nil)
;;   (setq ergoemacs-keyboard-layout "colemak")
;;   (ergoemacs-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes (quote ("7545d3bb77926908aadbd525dcb70256558ba05d7c478db6386bfb37fb6c9120" "9f3a4edb56d094366afed2a9ba3311bbced0f32ca44a47a765d8ef4ce5b8e4ea" default)))
 '(display-time-mode t)
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(indicate-buffer-boundaries (quote left))
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 90 :width normal)))))
