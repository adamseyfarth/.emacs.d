(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package paredit :ensure t)
(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode t))
(use-package cider :ensure t)
(use-package auto-complete :ensure t)
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
