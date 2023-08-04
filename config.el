(setq user-full-name "Yejun Su"
      user-mail-address "yejun@hey.com")

(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry nil)

(setq doom-font (font-spec :family "JetBrains Mono" :size 16)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 13))

(setq doom-theme 'doom-dracula)

(setq display-line-numbers-type t)

(setq org-directory "~/org/")

(use-package! org-habit
  :after org)

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("TAB"     . #'copilot-accept-completion)
              ("<tab>"   . #'copilot-accept-completion)
              ("C-TAB"   . #'copilot-accept-completion-by-word)
              ("C-<tab>" . #'copilot-accept-completion-by-word)))

(use-package! dash-at-point)

(map! :leader
      "sk" #'dash-at-point
      "sK" #'dash-at-point-with-docset)

(use-package! chatgpt-shell
  :custom
  (chatgpt-shell-model-version 2)
  (chatgpt-shell-welcome-function nil)
  (chatgpt-shell-openai-key (lambda () (auth-source-pick-first-password :host "api.openai.com"))))

(map! :leader
      :prefix ("z" . "chatgpt-shell")
      "z" #'chatgpt-shell
      "b" #'chatgpt-shell-prompt
      "c" #'chatgpt-shell-prompt-compose
      "s" #'chatgpt-shell-send-region
      "S" #'chatgpt-shell-send-and-review-region
      "e" #'chatgpt-shell-explain-code
      "r" #'chatgpt-shell-refactor-code)

(use-package! elixir-mode
  :hook (before-save . elixir-format-before-save)
  :config
  (defun elixir-format-before-save ()
    (when (derived-mode-p 'elixir-mode)
      (eglot-format-buffer))))

(use-package! nix-mode
  :hook (before-save . nix-format-before-save))

(defun yejun/launch-vanilla-emacs ()
  (interactive)
  (let ((default-directory "~/src/.emacs.d/"))
    (start-process "Emacs" nil "emacs" "-q" "-l" "init.el")))

(global-set-key (kbd "C-c e") #'yejun/launch-vanilla-emacs)

(defun yejun/gh-pr-create ()
  (interactive)
  (shell-command "gh pr create -w"))

(defun yejun/gh-pr-view ()
  (interactive)
  (shell-command "gh pr view -w"))

(map! :leader
      "gcp" #'yejun/gh-pr-create
      "gop" #'yejun/gh-pr-view)

(set-popup-rules!
  '(("^\\*chatgpt\\*" :side bottom :size 0.5 :select t)
    ("^ChatGPT>" :side bottom :size 0.5 :select t)))
