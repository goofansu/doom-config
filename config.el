(setq user-full-name "Yejun Su"
      user-mail-address "yejun@hey.com")

(setq doom-theme 'doom-dracula)

(setq doom-font (font-spec :family "JetBrains Mono" :size 16)
      doom-serif-font (font-spec :family "Courier New" :size 16)
      doom-variable-pitch-font (font-spec :family "Times New Roman" :size 16))

(setq org-directory "~/org/")

(use-package! elfeed
  :bind ("s-r" . elfeed))

(after! flycheck
  (delq 'idle-change flycheck-check-syntax-automatically))

(use-package! elixir-mode
  :hook (before-save . elixir-format-before-save)
  :config
  (defun elixir-format-before-save ()
    (when (derived-mode-p 'elixir-mode)
      (eglot-format-buffer))))

(use-package! nix-mode
  :hook (before-save . nix-format-before-save))

(use-package! gist
  :config
  (map! :leader
        :desc "Gist buffer/region" "cg" #'gist-region-or-buffer-private))

(use-package! chatgpt-shell
  :custom
  (chatgpt-shell-model-version 2)
  (chatgpt-shell-welcome-function nil)
  (chatgpt-shell-openai-key (lambda () (auth-source-pick-first-password :host "api.openai.com")))
  :config
  (map! :leader
        :prefix ("z" . "chatgpt-shell")
        "z" #'chatgpt-shell
        "b" #'chatgpt-shell-prompt
        "c" #'chatgpt-shell-prompt-compose
        "s" #'chatgpt-shell-send-region
        "S" #'chatgpt-shell-send-and-review-region
        "e" #'chatgpt-shell-explain-code
        "r" #'chatgpt-shell-refactor-code)

  (set-popup-rules!
    '(("^\\*chatgpt\\*" :side bottom :size 0.5 :select t)
      ("^ChatGPT>" :side bottom :size 0.5 :select t))))

(use-package! dash-at-point
  :config
  (map! :leader
        "sk" #'dash-at-point
        "sK" #'dash-at-point-with-docset))

(defun yejun/launch-vanilla-emacs ()
  (interactive)
  (let ((default-directory "~/src/.emacs.d/"))
    (start-process "Emacs" nil "emacs" "-q" "-l" "init.el" "config.org")))

(global-set-key (kbd "s-e") #'yejun/launch-vanilla-emacs)

(set-irc-server! "Libera Chat"
  '(:host "irc.libera.chat"
    :port 6697
    :nick "goofansu"
    :channels ("#emacs" "#elixir")
    :nickserv-password (lambda (server) (auth-source-pick-first-password :host server))))

(global-set-key (kbd "s-k") #'+irc/jump-to-channel)
