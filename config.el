;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Yejun Su"
      user-mail-address "yejun@hey.com"
      auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry nil)

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "JetBrains Mono" :size 16)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


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

;;; Keybindings
(map! :leader
      :prefix ("z" . "chatgpt-shell")
      "z" #'chatgpt-shell
      "b" #'chatgpt-shell-prompt
      "c" #'chatgpt-shell-prompt-compose
      "s" #'chatgpt-shell-send-region
      "S" #'chatgpt-shell-send-and-review-region
      "e" #'chatgpt-shell-explain-code
      "r" #'chatgpt-shell-refactor-code)

(map! :leader
      "gcp" #'yejun/gh-pr-create
      "gop" #'yejun/gh-pr-view)

;;; Pop-up rules
(set-popup-rules!
  '(("^\\*chatgpt\\*" :side bottom :size 0.5 :select t)
    ("^ChatGPT>" :side bottom :size 0.5 :select t)))

;;; Packages
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("TAB" . 'copilot-accept-completion)
              ("<tab>" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(use-package! chatgpt-shell
  :defer t
  :commands (chatgpt-shell-prompt
             chatgpt-shell-prompt-compose
             chatgpt-shell-send-region
             chatgpt-shell-send-and-review-region
             chatgpt-shell-explain-code
             chatgpt-shell-refactor-code)
  :custom
  (chatgpt-shell-model-version 2)
  (chatgpt-shell-welcome-function nil)
  (chatgpt-shell-openai-key (lambda () (auth-source-pass-get 'secret "openai/api-key/chatgpt-shell"))))

(use-package! elixir-mode
  :hook (before-save . elixir-format-before-save)
  :config
  (defun elixir-format-before-save ()
    (when (derived-mode-p 'elixir-mode)
      (eglot-format-buffer))))

(use-package! nix-mode
  :hook (before-save . nix-format-before-save))

(use-package org-habit
  :after org)

;;; Functions
(defun yejun/launch-rune-emacs ()
  (interactive)
  (let ((default-directory "~/src/rune/"))
    (start-process "Emacs" nil "emacs" "-q" "-l" "init.el")))

(global-set-key (kbd "C-c e") #'yejun/launch-rune-emacs)

(defun yejun/gh-pr-create ()
  (interactive)
  (shell-command "gh pr create -w"))

(defun yejun/gh-pr-view ()
  (interactive)
  (shell-command "gh pr view -w"))
