;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; See https://tecosaur.github.io/emacs-config/config.html
;; Personal Information
(setq user-full-name "Yejun Su"
      user-mail-address "yejun@hey.com")

(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry nil) ; default is 7200 (2h)

;; Simple settings
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      scroll-preserve-screen-position 'always     ; Don't have `point' jump around
      scroll-margin 2)                            ; It's nice to maintain a little margin

(display-time-mode 1)                             ; Enable time in the mode-line

(global-subword-mode 1)                           ; Iterate through CamelCase words
(menu-bar-mode -1)                                ; Hide menu bar

;; Frame sizing
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))

;; Auto-customisations
(setq-default custom-file (expand-file-name ".custom.el" doom-emacs-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; Windows
(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

;; Buffer defaults
(setq-default major-mode 'org-mode)
(setq-default doom-scratch-initial-major-mode 'org-mode)

;; Visual Settings
(setq display-line-numbers-type t)

(setq doom-font (font-spec :family "JetBrains Mono" :size 16)
      doom-big-font (font-spec :family "JetBrains Mono" :size 28)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 16)
      doom-unicode-font (font-spec :family "JuliaMono")
      doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light))

(defvar required-fonts '("JetBrainsMono.*" "Overpass" "JuliaMono" "IBM Plex Mono"))

(defvar available-fonts
  (delete-dups (or (font-family-list)
                   (split-string (shell-command-to-string "fc-list : family")
                                 "[,\n]"))))

(defvar missing-fonts
  (delq nil (mapcar
             (lambda (font)
               (unless (delq nil (mapcar (lambda (f)
                                           (string-match-p (format "^%s$" font) f))
                                         available-fonts))
                 font))
             required-fonts)))

(if missing-fonts
    (pp-to-string
     `(unless noninteractive
        (add-hook! 'doom-init-ui-hook
          (run-at-time nil nil
                       (lambda ()
                         (message "%s missing the following fonts: %s"
                                  (propertize "Warning!" 'face '(bold warning))
                                  (mapconcat (lambda (font)
                                               (propertize font 'face 'font-lock-variable-name-face))
                                             ',missing-fonts
                                             ", "))
                         (sleep-for 0.5))))))
  ";; No missing fonts detected")

;; Theme and modeline
(setq doom-theme 'doom-dracula)
(remove-hook 'window-setup-hook #'doom-init-theme-h)
(add-hook 'after-init-hook #'doom-init-theme-h 'append)
(delq! t custom-theme-load-path)

(defun doom-modeline-conditional-buffer-encoding ()
  "Only show the modeline when encoding is not LF UTF-8."
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

;; projectile
(setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Key bindings
(map! :leader
      (:prefix-map ("c" . "code")
       :desc "Create private Gist for region or buffer" "g" #'gist-region-or-buffer-private)
      (:prefix-map ("s" . "search")
       :desc "Look up in Dash" "k" #'dash-at-point
       :desc "Look up in Dash (w/ prompt)" "K" #'dash-at-point-with-docset))

;; Functions
(defun gh-pr-create ()
  (interactive)
  (shell-command "gh pr create -w"))

(defun gh-pr-view ()
  (interactive)
  (shell-command "gh pr view -w"))
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

;; Packages
(use-package! elixir-mode
  :hook (before-save . elixir-format-before-save)
  :config
  (defun elixir-format-before-save ()
    (when (derived-mode-p 'elixir-mode)
      (lsp-format-buffer))))

(use-package! nix-mode
  :hook (before-save . nix-format-before-save))

(after! magit
  (setq magit-repository-directories '(("~/src" . 2))))

(use-package! ruby-mode
  :init
  (setq ruby-indent-level 2)
  :hook (ruby-mode . ruby-electric-mode))

(use-package! wakatime-mode
  :config
  (global-wakatime-mode))

(use-package! web-mode
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

;; Org mode
(setq org-directory "~/org"
      org-roam-directory "~/org")

(use-package! org
  :hook (org-mode . auto-revert-mode))

(after! org
  (setq org-use-property-inheritance t
        org-log-done 'time
        org-log-repeat 'note
        org-list-allow-alphabetical t
        org-fold-catch-invisible-edits 'smart)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")
          (sequence "READING(r)" "TOREVIEW(v!/!)" "|" "READ(R!/!)") ;; From https://github.com/dsdshcym/.emacs.d/blob/master/lisp/init-org.el
          (sequence "DRAFT(f)" "|" "PUBLISHED(P!/!)")))
  (setq org-capture-templates
        '(;; Areas
          ("t" "Personal" entry
           (file+headline +org-capture-todo-file "Personal")
           "* TODO %?\n%i\n%a" :prepend t)
          ("f" "Family" entry
           (file+headline +org-capture-todo-file "Family")
           "* TODO %?\n%i\n%a" :prepend t)
          ("w" "Work" entry
           (file+headline +org-capture-todo-file "Work")
           "* TODO %?\n%i\n%a" :prepend t)
          ;; Resources
          ("r" "Inbox" entry
           (file "~/org/inbox.org")
           "* %?\n%U\n" :prepend nil)
          ;; Projects
          ("p" "Projects")
          ("pt" "Project todo" entry
           #'+org-capture-central-project-todo-file
           "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
          ("pn" "Project notes" entry
           #'+org-capture-central-project-notes-file
           "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
          ))
  (setq org-tag-persistent-alist
        '((:startgroup)
          ("work" . ?w)
          (:endgroup)
          (:startgroup)
          ("article" . ?a)
          ("book" . ?b)
          ("course" . ?c)
          ("podcast" . ?d)
          ("video" . ?v)
          (:endgroup)
          ))
  (setq org-agenda-custom-commands
        '(("y" agenda*)
          ("w" todo "WAIT")
          ("W" todo-tree "WAIT")
          (" " . "Saved searches")
          (" p" "Projects"
           ((agenda "" ((org-agenda-files '("~/org/projects.org"))))))
          (" s" "Study"
           ((todo "READING|TOREVIEW|READ" ((org-agenda-overriding-header "Reading")))
            (todo "DRAFT|PUBLISHED" ((org-agenda-overriding-header "Writing")))))
          ))
  ;; See https://www.nicklanasa.com/posts/productivity-setup
  (defmacro func-ignore (fnc)
    "Return function that ignores its arguments and invokes FNC."
    `(lambda (&rest _rest)
       (funcall ,fnc)))
  (advice-add 'org-deadline       :after (func-ignore #'org-save-all-org-buffers))
  (advice-add 'org-schedule       :after (func-ignore #'org-save-all-org-buffers))
  (advice-add 'org-store-log-note :after (func-ignore #'org-save-all-org-buffers))
  (advice-add 'org-todo           :after (func-ignore #'org-save-all-org-buffers))
  ;; Tracking habits
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-show-all-today nil))

(after! org-roam
  (setq org-roam-completion-everywhere t)
  (org-roam-db-autosync-mode)
  (defun org-roam--insert-timestamp ()
    (org-entry-put nil "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]")))
  (add-hook 'org-roam-capture-new-node-hook #'org-roam--insert-timestamp)
  (setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?\n%U\n"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n")))))
