;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         General Emacs Apperance                  ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode 1)
(setq inhibit-startup-message t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
;; https://www.emacswiki.org/emacs/LoadPath
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path)) ;recursive append
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package sml-mode
	     :ensure t
	     :config
	     (setenv "PATH" (concat "/usr/local/smlnj/bin:" (getenv "PATH")))
	     (setq exec-path (cons "/usr/local/smlnj/bin" exec-path)))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Org-mode stuff
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; (use-package company
;; 	     :ensure t
;; 	     :config
;; 	     (global-company-mode))

(use-package ivy
	     :ensure t)

(use-package counsel
	     :ensure t)

;; evil mode!!!!
(use-package evil
  :ensure t
  :bind (:map evil-normal-state-map
	      ("C-u" . evil-scroll-up))
  :config
  (evil-mode 1))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-numbers
	     :ensure t
	     :bind ( :map evil-normal-state-map
			  ("C-a" . evil-numbers/inc-at-pt)
			  ("C-x" . evil-numbers/dec-at-pt)
			  :map evil-visual-state-map
			  ("C-a" . evil-numbers/inc-at-pt)
			  ("C-x" . evil-numbers/dec-at-pt)))
(use-package evil-leader
	     :ensure t
	     :config
	     (evil-leader-mode 1)
	     (evil-leader/set-leader "<SPC>"))

(use-package evil-snipe
	     :ensure t
	     :config
	     (evil-snipe-mode 1))

(use-package evil-commentary
	     :ensure t
	     :config
	     (evil-commentary-mode))

(use-package evil-visualstar
	     :ensure t
	     :config
	     (global-evil-visualstar-mode t))

(load "evil-unimpaired.el")
(evil-unimpaired-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         Colors                                   ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The default is zenburn, a popular color setup for emacs. There are
;; other colors if you don't want zenburn. See color-init.el for more
;; options.
(load "color-init.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         MIT-scheme config                        ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the place where you have installed scheme. Be sure to set
;; this to an appropriate value!!!
(setq scheme-root "/usr/local")

(setq scheme-program-name
      (concat
       scheme-root "/bin/mit-scheme "
       "--heap 10000"))

;; generic scheme completeion
(require 'scheme-complete)
(autoload 'scheme-smart-complete "scheme-complete" nil t)
(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
(setq lisp-indent-function 'scheme-smart-indent-function)

;; mit-scheme documentation
(require 'mit-scheme-doc)

;; Special keys in scheme mode. Use <tab> to indent scheme code to the
;; proper level, and use M-. to view mit-scheme-documentation for any
;; symbol.
(eval-after-load
 'scheme
 '(define-key scheme-mode-map "\t" 'scheme-complete-or-indent))

(eval-after-load
 'cmuscheme
 '(define-key inferior-scheme-mode-map "\t" 'scheme-complete-or-indent))

(eval-after-load
 'scheme
 '(define-key scheme-mode-map (kbd "M-.") 'mit-scheme-doc-lookup))

(eval-after-load
 'cmuscheme
 '(define-key inferior-scheme-mode-map (kbd "M-.")
    'mit-scheme-doc-lookup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         Flash Paren Mode                         ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "flash-paren")
(flash-paren-mode 1)
(setq flash-paren-delay 0.1)

;; ;; game???
;; (require 'malyon)
;; (defun zork ()
;;   (interactive)
;;   (malyon (locate-file "zork1.z5" load-path)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (counsel ivy company which-key use-package try sml-mode org-bullets evil-visualstar evil-surround evil-snipe evil-numbers evil-leader evil-commentary))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
