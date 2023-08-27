(setq user-full-name "Yejun Su"
      user-mail-address "yejun@hey.com")

(if (display-graphic-p)
    (setq doom-theme 'modus-operandi)
  (setq doom-theme 'modus-vivendi))

(use-package modus-themes
  :config
  (setq modus-themes-completions
        '((matches . (extrabold underline))
          (selection . (semibold italic)))))

(setq doom-font (font-spec :family "JetBrains Mono" :size 16)
      doom-serif-font (font-spec :family "Courier New" :size 16)
      doom-variable-pitch-font (font-spec :family "Times New Roman" :size 16))

(setq org-directory "~/Documents/org/")

(after! org
  (setq org-log-into-drawer t)
  (setq org-log-done 'time)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "HOLD(h@/!)" "|" "DONE(d!)" "KILL(k@)")))
  (setq org-todo-keyword-faces
        '(("HOLD" . +org-todo-onhold)
          ("KILL" . +org-todo-cancel)))

  (setq org-capture-templates
        '(("t" "Tasks" entry (file+headline +org-capture-todo-file "Inbox") "* TODO %?\n%i\n%a" :prepend t)
          ("n" "Notes" entry (file +org-capture-notes-file) "* %?\n" :prepend t)
          ("j" "Journal" entry (file+olp+datetree +org-capture-journal-file) "* %U %?\n%i\n%a")
          ("b" "BrightU" entry (file "brightu.org") "* TODO %?\n%i\n%a")))

  (setq org-agenda-block-separator (string-to-char "="))
  (setq org-agenda-custom-commands
        '(("d" "My daily agenda"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "High-priority unfinished tasks:")))
            (agenda "" ((org-agenda-span 1)
                        (org-agenda-start-day "+0d")))
            (alltodo ""
                     ((org-agenda-overriding-header "ALL normal priority tasks:")
                      (org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                     (air-org-skip-subtree-if-priority ?A)
                                                     (org-agenda-skip-if nil '(scheduled deadline)))))))))))

;; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(use-package! org-roam
  :after org)

(map! :leader
      :desc "Capture to roam today" "nn" #'org-roam-dailies-capture-today
      :desc "Go to roam today" "nN" #'org-roam-dailies-goto-today
      :desc "Capture to roam node" "nd" #'org-roam-capture
      :desc "Go to roam node" "nF" #'org-roam-node-find)

(use-package! org-habit
  :after org)

(use-package! org-pandoc-import :after org)

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

(use-package! forge
  :config
  (setq forge-topic-list-limit '(20 . 5)))

(defun yejun/gh-pr-create ()
  (interactive)
  (shell-command "gh pr create -w"))

(defun yejun/gh-pr-view ()
  (interactive)
  (shell-command "gh pr view -w"))

(defun yejun/gist-region-or-buffer (&optional p)
  (interactive "P")
  (let ((filename (buffer-name))
        (output-buffer " *gist-output*")
        (public (if p " --public" "")))
    (shell-command-on-region
     (if (use-region-p) (region-beginning) (point-min))
     (if (use-region-p) (region-end) (point-max))
     (concat "gh gist create --filename " filename public " -")
     output-buffer)
    (with-current-buffer output-buffer
      (goto-char (point-max))
      (previous-line)
      (kill-new (thing-at-point 'line)))
    (kill-buffer output-buffer)))

(map! :leader
      :desc "Gist buffer/region" "cg" #'yejun/gist-region-or-buffer)

(use-package! chatgpt-shell
  :custom
  (chatgpt-shell-model-version 2)
  (chatgpt-shell-welcome-function nil)
  (chatgpt-shell-openai-key (lambda () (auth-source-pick-first-password :host "api.openai.com")))

  :config
  (set-popup-rules!
    '(("^\\*chatgpt\\*" :side bottom :size 0.5 :select t)
      ("^ChatGPT>" :side bottom :size 0.5 :select t))))

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
      "sk" #'dash-at-point
      "sK" #'dash-at-point-with-docset)

(defun yejun/launch-vanilla-emacs ()
  (interactive)
  (let ((default-directory "~/src/.emacs.d/"))
    (start-process "Emacs" nil "emacs" "-q" "-l" "init.el" "config.org")))

(global-set-key (kbd "C-c e") #'yejun/launch-vanilla-emacs)

(set-irc-server! "Libera Chat"
  '(:host "irc.libera.chat"
    :port 6697
    :nick "goofansu"
    :channels ("#emacs" "#elixir")
    :nickserv-password (lambda (server) (auth-source-pick-first-password :host server))))

(global-set-key (kbd "s-k") #'+irc/jump-to-channel)
