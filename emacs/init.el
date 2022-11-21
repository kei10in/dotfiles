;;;
;;; use-package
;;;
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;;; Register private configuration pathes to load-path
(setq load-path
      (append (list
               (expand-file-name "~/.emacs.d/lisp/")
               (expand-file-name "~/.emacs.d/site-lisp/")
               (expand-file-name "~/.emacs.d/conf/")
               )
              load-path))

;;; User Informations
(setq user-full-name "Keisuke Ijuin")
(setq user-mail-address "kei10in@gmail.com")

(defvar private-lib-dir "~/.emacs.d/")
(defvar private-lisp-dir (concat private-lib-dir "./lisp/"))
(defvar private-conf-dir (concat private-lib-dir "./conf/"))

(require 'detectenv)


;;; Default major mode
(setq major-mode 'text-mode)

;;; デバッグ モード
(setq debug-on-error nil)

;;; Set file search path
(dolist (dir (list
              (expand-file-name "~/local/bin")
              (expand-file-name "~/local/sbin")
              "/opt/local/bin"
              "/opt/bin"
              "/usr/local/bin"
              "/usr/bin"
              "/bin"
              "/opt/local/sbin"
              "/opt/sbin"
              "/usr/local/sbin"
              "/usr/sbin"
              "/sbin"
              "/usr/X11R6/bin"
              ))
  ;; PATH と exec-path に同じ物を追加します
  (when (and (file-exists-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))


;;;============================================================================;
;;; Global Appearances
;;; 各 Windows mode に関わる設定は別ファイルを参照
;;;============================================================================;
(set-face-attribute 'default nil :font "HackGen-15")

;;; Hide startup message.
(setq inhibit-startup-message t)

;;; Set initial message for *scratch* buffer.
(setq initial-scratch-message "")

;;; Menu bar / Tool bar
(when (not run-no-window)
  (menu-bar-mode -1)
  (tool-bar-mode 0))

(setq column-number-mode t)

;;; Cursor configurations
(blink-cursor-mode t)
;; (setq cursor-in-non-selected-windows t)


;;; Text Width Configurations
;; Truncate lines
(setq truncate-lines t)
;; Truncate-lines in partial window
(setq truncate-partial-width-windows t)


;;; Show clock
;; (setq display-time-day-and-date t
;;       display-time-24hr-format t)
;; (setq display-time-format "%m/%d(%a) %H:%M")
;; (display-time)


;;; Stop visible bell
(setq visible-bell nil)


;;; Tab indent configurations
;; Use SPC as TAB
(setq-default indent-tabs-mode nil)
;; Tab Width
(setq-default tab-width 4)
;; Follow before line indentation
(setq indent-line-function 'indent-relative-maybe)


;;;------------------------------------;
;;; line height
;;; 整数で指定するとピクセル数で、
;;; 少数で指定すると行の高さに対して
;;; 相対値で設定されます。
;;;------------------------------------;
(setq-default line-spacing 0.1)


;;; Use dialog on gui manipulation.
(setq use-dialog-box t)


;;; Change buffer name display format
(require 'uniquify)
;; forward:        bar/mumble/name  quux/mumble/name
;; reverse:        name\\mumble\\bar  name\\mumble\\quux
;; post-forward:   name|bar/mumble  name|quux/mumble
;; post-forward-angle-brackets:   name<bar/mumble>  name<quux/mumble>
;; nil:            name  name<2>
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; ignore special buffer
(setq uniquify-ignore-buffers-re "*[^*]+*")


;;; Key bindings:
;; Modify C-h to Backspace.
(defun terminal-init-bobcat ()
  "Terminal initialization function for bobcat."
  ;; HP terminals usually encourage using ^H as the rubout character
  (keyboard-translate ?\177 ?\^h)
  (keyboard-translate ?\^h ?\177))
(terminal-init-bobcat)

;; Delete region by C-h
(delete-selection-mode 1)

;; Modify C-% to query-replace-regexp
(global-set-key (read-kbd-macro "C-%") 'query-replace-regexp)
(global-set-key (kbd "C-c :") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c <tab>") 'ff-find-other-file)
(global-unset-key (kbd "C-z"))

;;;
;;; Global text scale mode
;;;
(define-globalized-minor-mode
  global-text-scale-mode
  text-scale-mode
  (lambda () (text-scale-mode 1)))
(defun global-text-scale-adjust (inc) (interactive "p")
  (if (= inc 0)
      (progn
       (global-text-scale-adjust (- text-scale-mode-amount))
       (global-text-scale-mode -1))
    (progn
     (text-scale-set 1)
     (kill-local-variable 'text-scale-mode-amount)
     (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
     (global-text-scale-mode 1))))
(defun global-text-scale-reset () (interactive) (global-text-scale-adjust 0))
(defun global-text-scale-increase (inc) (interactive "p")
  (global-text-scale-adjust inc))
(defun global-text-scale-decrease (dec) (interactive "p")
  (global-text-scale-increase (- dec)))
(global-set-key (kbd "M-0") 'global-text-scale-reset)
(global-set-key (kbd "M-=") 'global-text-scale-increase)
(global-set-key (kbd "M--") 'global-text-scale-decrease)
(global-set-key (kbd "<M-wheel-up>") 'global-text-scale-increase)
(global-set-key (kbd "<M-wheel-down>") 'global-text-scale-decrease)


;; Find File
;; C-x C-f を find-file-at-point にする
(require 'find-file)
(global-set-key (kbd "C-c <tab>") 'ff-find-other-file)
(ffap-bindings)
(setq ff-other-file-alist
      '(
        ("\\.[hH][pP][pP]$" (".cpp" ".CPP" ".cxx" ".CXX"))
        ("\\.[cC][pP][pP]$" (".hpp" ".HPP" ".h"))

        ("\\.[hH][hH]$"  (".cc" ".CC"))
        ("\\.[cC][cC]$"  (".hh" ".HH" ".h" ".H"))

        ("\\.[cC][xX][xX]$" (".hh" ".h"))
        
        ("\\.[hH]$"   (".cpp" ".CPP" ".cc" ".CC" ".c" ".C" ".cxx" ".m" ".mm"))
        ("\\.[cC]$"   (".h" ".H"))
        
        ("\\.mm?$" (".h"))
        ))


;; help
(global-set-key "\C-x?" 'help)
(global-set-key "\M-?" 'help-for-help)


;;;; Delete new-line char by kill-line (t)
;; (setq kill-whole-line t)


;;; Mouse wheel configurations
;; Mouse wheel scroll speed
(setq mouse-wheel-scroll-amount '(3 ((shift) . 10) ((control) . nil)))
;; Mouse wheel acceleration
(setq mouse-wheel-progressive-speed nil) 


;;; emacs server
;; (server-start)


;; Auto backup
(setq make-backup-files t)


;;; Auto Save
;; Create auto save files
(setq auto-save-default nil)
;; Delete auto saved files
(setq delete-auto-save-files nil)
;; Don't make ~/.saves-PID-hostname
(setq auto-save-list-file-prefix nil)


;;; Confirm when exiting emacs
(setq confirm-kill-emacs 'y-or-n-p)


;;; To editable compressed files and prevent info written in Japanese
(auto-compression-mode t)


;;; Completion:
;; http://d.hatena.ne.jp/khiker/20061220/1166643421
;; ignore directories
(add-to-list 'completion-ignored-extensions ".svn/")
;; ignore cases
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)


;;; Emacs lock:
;; disable kill-buffer *scratch*
(require 'emacs-lock)
(save-excursion
  (set-buffer "*scratch*")
  (emacs-lock-mode 'kill))


;;; Highlight line
(require 'hl-line)
(global-hl-line-mode)


;;;------------------------------------;
;;; Color Configurations
;;;------------------------------------;
;;; Enable syntax highlight
(global-font-lock-mode t)
(require 'font-lock)

;;; Highlight parentheses, braces, brackets.
;;; You can use C-M-p or C-M-n to jump to corresponding paren
(show-paren-mode t)
;; カッコ対応表示のスタイル
;; カッコその物に色が付く(デフォルト)
;; (setq show-paren-style 'parenthesis)
;; カッコ内に色が付く
;; (setq show-paren-style 'expression)
;; 画面内に収まる場合はカッコのみ、画面外に存在する場合はカッコ内全体に色が付く
;; (setq show-paren-style 'mixed)

;;; Highlight region
(setq transient-mark-mode t)

;;; Highlight matches
(setq search-highlight t)

;;; Highlight matches on interactive replace
(setq query-replace-highlight t)


;;; Emacs runtime environment
; (require 'init-runtime-environment)

; (load-theme 'classical-console t)


;;; Cocoa Emacs
(when run-cocoa-emacs
  ;; swap Commmand and Option
  (setq ns-command-modifier 'meta)
  (setq ns-alternate-modifier 'super)
  ;; Open file by Drag & Drop
  (define-key global-map [ns-drag-file] 'ns-find-file)
  ;; Don't create new frame when Drag & Droped
  (setq ns-pop-up-frames nil)

  ;; Set frame Transparency
  ;; (set-frame-parameter (selected-frame) 'alpha '(90 80))
  (add-to-list 'default-frame-alist '(alpha . (98 90)))
  (add-to-list 'default-frame-alist '(width . 256))
  (add-to-list 'default-frame-alist '(height . 100))
  )


;;;
;;; Theme
;;;
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))


;;;
;;; line number
;;;

;;; Show line and column numbers in mode line
(line-number-mode t)
(column-number-mode t)

(use-package nlinum
  :init
  (global-nlinum-mode)
  (setq nlinum-format "%4d")
  )

;;;
;;; Vertico / Consult
;;;

;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))


;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

;; Enable rich annotations using the Marginalia package
;; https://github.com/minad/marginalia
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; orderless
;; https://github.com/oantolin/orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


;;;
;;; helm
;;;

;; (use-package helm
;;   ;; :ensure 1
;;   :init
;;   (require 'helm-config)
;;   (helm-mode 1)

;;   (global-set-key (kbd "C-x b") 'helm-for-files)
;;   (global-set-key (kbd "M-x") 'helm-M-x)
;;   (global-set-key (kbd "C-x C-f") 'helm-find-files)

;;   (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
;;   (define-key helm-map (kbd "C-z") 'helm-select-action)

;;   (add-to-list 'helm-completing-read-handlers-alist '(find-file-at-point . nil))
;;   (add-to-list 'helm-completing-read-handlers-alist '(dired-at-point . nil))

;;   (setq helm-ff-auto-update-initial-value nil)
;;   (setq helm-idle-delay 0.001)
;;   (setq helm-input-idle-delay 0.001)
;;   (setq helm-buffer-max-length 40)

;;   (defvar my/helm-source-files-in-current-dir
;;     `((name . "Files from Current Directory")
;;       (candidates . (lambda ()
;;                       (with-helm-current-buffer
;;                         (let ((dir (helm-current-directory)))
;;                           (when (file-accessible-directory-p dir)
;;                             (directory-files dir nil))))))
;;       (keymap . ,helm-generic-files-map)
;;       (no-delay-on-input)
;;       (help-message . helm-generic-file-help-message)
;;       (mode-line . helm-generic-file-mode-line-string)
;;       (type . file)))

;;   (setq helm-for-files-preferred-list
;;         '(helm-source-buffers-list
;;           my/helm-source-files-in-current-dir
;;           helm-source-recentf
;;           helm-source-bookmarks
;;           helm-source-file-cache
;;           helm-source-buffer-not-found
;;           )))

;;;
;;; Ouput by Emacs
;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("24168c7e083ca0bbc87c68d3139ef39f072488703dcdd82343b8cab71c0f62a7" "9abe2b502db3ed511fea7ab84b62096ba15a3a71cdb106fd989afa179ff8ab8d" default))
 '(package-selected-packages
   '(nlinum marginalia orderless consult helm vertico use-package monokai-pro-theme monokai-theme solarized-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
