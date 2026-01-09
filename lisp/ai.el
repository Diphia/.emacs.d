;;; ai.el --- Helpers for summoning Claude agents -*- lexical-binding: t; -*-

;;; Commentary:
;; Interactive helpers for launching Claude workflows from Emacs.

;;; Code:

(require 'subr-x)

(defvar agent-summon--prompt-buffer-name "*Agent Prompt*"
  "Name of the prompt buffer used by `agent-summon'.")

(defvar-local agent-summon--pending-directory nil
  "Directory captured when preparing the Codex prompt.")

(defconst agent-summon-commit-prompt
  "You are operating inside this repository. Stage every unstaged change, write a clear commit message that captures the overall intent, create the commit, and push to the current branch. You don't need to ask for permission before committing, go straight and commit then push. After pushing, summarize what happened and list follow-up actions if anything fails."
  "Prompt used by `agent-summon-commit-and-push'.")

(define-derived-mode agent-summon-prompt-mode text-mode "Agent-Prompt"
  "Mode for collecting a prompt before launching Codex."
  (setq-local header-line-format "C-c C-c to launch, C-c C-k to abort.")
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'agent-summon--submit)
    (define-key map (kbd "C-c C-k") #'agent-summon--abort)
    (use-local-map map)))

(defun agent-summon (&optional preset-prompt)
  "Launch Claude in a macOS Terminal for the current buffer's directory.

If PRESET-PROMPT is provided (interactively or via Lisp), Claude is
invoked immediately using that prompt.  Otherwise a dedicated buffer
is opened so the user can compose a prompt which is submitted with
`C-c C-c'."
  (interactive)
  (let* ((dir (agent-summon--buffer-directory))
         (prompt (and preset-prompt (string-trim preset-prompt))))
    (if (and prompt (not (string-empty-p prompt)))
        (agent-summon--launch dir prompt)
      (agent-summon--prepare-prompt dir))))

(defun agent-summon-commit-and-push ()
  "Open Claude to stage, commit, and push the current repository."
  (interactive)
  (agent-summon agent-summon-commit-prompt))

(defun agent-summon--prepare-prompt (directory)
  "Open a transient buffer to collect a prompt for DIRECTORY."
  (let ((buffer (get-buffer-create agent-summon--prompt-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (agent-summon-prompt-mode)
      (setq agent-summon--pending-directory directory))
    (pop-to-buffer buffer)
    (goto-char (point-min))
    (message "Compose Claude prompt, then press C-c C-c to launch.")))

(defun agent-summon--submit ()
  "Finalize the prompt buffer and launch Claude."
  (interactive)
  (let ((prompt (string-trim (buffer-string)))
        (dir agent-summon--pending-directory))
    (unless dir
      (user-error "No directory recorded for this prompt buffer"))
    (kill-buffer)
    (agent-summon--launch dir prompt)))

(defun agent-summon--abort ()
  "Abort the prompt entry without launching Codex."
  (interactive)
  (kill-buffer)
  (message "Agent summon cancelled."))

(defun agent-summon--launch (directory prompt)
  "Launch Codex from DIRECTORY with PROMPT."
  (let ((command (agent-summon--build-command directory prompt)))
    (agent-summon--run-applescript
     (format "tell application \"Terminal\"
activate
do script \"%s\"
end tell"
             (agent-summon--osascript-escape command)))))

(defun agent-summon--build-command (directory prompt)
  "Create the shell COMMAND to run Claude in DIRECTORY with PROMPT."
  (let* ((expanded (expand-file-name directory))
         (cd-part (format "cd %s" (shell-quote-argument expanded))))
    (if (string-empty-p (string-trim (or prompt "")))
        (format "%s && claude --dangerously-skip-permissions" cd-part)
      (format "%s && claude --dangerously-skip-permissions \"%s\""
              cd-part
              (agent-summon--shell-escape-argument prompt)))))

(defun agent-summon--buffer-directory ()
  "Return the directory to operate from for the current buffer."
  (or (and buffer-file-name (file-name-directory buffer-file-name))
      default-directory
      (user-error "Cannot determine directory for current buffer")))

(defun agent-summon--osascript-escape (string)
  "Escape STRING for embedding inside an AppleScript quoted literal."
  (setq string (string-replace "\\" "\\\\" string))
  (string-replace "\"" "\\\"" string))

(defun agent-summon--shell-escape-argument (string)
  "Escape STRING for safe use inside shell double quotes."
  (setq string (string-replace "\\" "\\\\" string))
  (setq string (string-replace "\"" "\\\"" string))
  (setq string (string-replace "$" "\\$" string))
  (string-replace "`" "\\`" string))

(defun agent-summon--run-applescript (script)
  "Evaluate AppleScript SCRIPT via `do-applescript'."
  (unless (fboundp 'do-applescript)
    (user-error "AppleScript integration is unavailable in this Emacs build"))
  (do-applescript script)
  (message "Claude launched in Terminal."))

(provide 'ai)

;;; ai.el ends here
