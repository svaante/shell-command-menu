;;; shell-command-menu.el -- Manage and launch shell commands by menu -*- lexical-binding: t -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Daniel Pettersson
;; Maintainer: Daniel Pettersson <daniel@dpettersson.net>
;; Created: 2023
;; License: GPL-3.0-or-later
;; Version: 0.0.1
;; Homepage: https://github.com/svaante/shell-command-menu
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Manage and launch shell commands by menu.

;;   (shell-command-menu-monitor-mode 1)
;;   M-x shell-command-menu

;;; Code:

(require 'cl-lib)
(require 'tramp)
(require 'shell)
(require 'proced) ; face


;;; Custom

(defgroup shell-command-menu nil
  "Manage and launch shell commands by menu."
  :prefix "shell-command-menu-")

(defcustom shell-command-menu-modes
  '((shell-command-mode async-shell-command shell-command-mode-hook)
    (compilation-mode   compile             compilation-start-hook))
  "Alist of (MODE COMMAND HOOK)."
  :type '(alist :key-type symbol
                :value-type (group (symbol :tag "Command")
                                   (symbol :tag "Hook"))))


;;; Internals

(defvar shell-command-menu-items nil)

(cl-defstruct (shell-command-menu--item
               (:constructor shell-command-menu--item-make))
   name command directory start-time end-time exit-status)

(defun shell-command-menu--file-name (item)
  (with-slots (name start-time) item
    (file-name-concat
     temporary-file-directory
     (concat (car (last (file-name-split (car (split-string name)))))
             "-"
             (format-time-string "%Y%m%d-%H%M%S.out"
                                 (seconds-to-time start-time))))))

(defvar-local shell-command-menu--append-point 1)

(defun shell-command-menu--append (file)
  (when (> (point-max) shell-command-menu--append-point)
    (let ((coding-system-for-write 'raw-text))
      (write-region shell-command-menu--append-point (point-max)
                    file 'append 'no-echo))
    (setq-local shell-command-menu--append-point (point-max))))

(defun shell-command-menu--poll (timer process)
  (when-let* ((item (process-get process 'shell-command-menu-item)))
    (when-let* ((buffer (process-buffer process))
                ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (shell-command-menu--append (shell-command-menu--file-name item))))
    (when (memq (process-status process) '(exit signal))
      (cancel-timer timer)
      (with-slots (exit-status end-time) item
        (setf exit-status (process-exit-status process)
              end-time (time-to-seconds)))
      (shell-command-menu nil))))

(defun shell-command-menu--hook (&rest _)
  (when-let* ((process (get-buffer-process (current-buffer)))
              (command (or (process-get process 'remote-command)
                           (process-command process)))
              (name (string-join
                     (if (equal (nth 1 command) shell-command-switch)
                         (nthcdr 2 command)
                       command)
                     " ")))
    (when-let* ((tramp-connection-entry
                 (process-get process 'shell-command-menu-item)))
      ;; HACK: If under tramp file handler this will called twice,
      ;; first with the connection handle then with the "fake"
      ;; process.  We are not interested in the connection process.
      (setq shell-command-menu-items
            (delq tramp-connection-entry shell-command-menu-items)))
    ;; Setup command metadata
    (let ((item
           (shell-command-menu--item-make
            :name name
            :command (cadr (assoc major-mode shell-command-menu-modes))
            :directory (abbreviate-file-name default-directory)
            :start-time (time-to-seconds (current-time)))))
      (push item shell-command-menu-items)
      ;; If buffer is suddenly killed, rescue contents into log file
      (add-hook 'kill-buffer-hook
                (apply-partially #'shell-command-menu--append
                                 (shell-command-menu--file-name item))
                nil t)
      ;; Store handle at process for poll use
      (process-put process 'shell-command-menu-item item))
    ;;
    (shell-command-menu nil)
    ;; Start polling process for process output and exit status
    (let ((timer (timer-create)))
      (timer-set-time timer nil 1)
      (timer-set-function timer #'shell-command-menu--poll
                          (list timer process))
      (timer-activate timer))))


;;; Supporting cast

(defun shell-command-menu-run (&optional arg)
  "Re run shell command.
If ARG is non-nil skip quitting the menu."
  (interactive "P" shell-command-menu)
  (with-slots (name directory command) (tabulated-list-get-id)
    (unless arg (quit-window))
    (unless command
      (user-error "No associated `command' for `%s'" name))
    (let ((default-directory (or directory default-directory)))
      (funcall command (substring-no-properties name)))))

(defun shell-command-menu-edit (arg)
  "Edit shell command in minibuffer.
If ARG is non-nil skip quitting the menu."
  (interactive "P" shell-command-menu)
  (with-slots (name directory command) (tabulated-list-get-id)
    (unless arg (quit-window))
    (unless command
      (user-error "No associated `command' for `%s'" name))
    (minibuffer-with-setup-hook
        (lambda ()
          (delete-region (minibuffer-prompt-end) (point-max))
          (insert name))
      (let ((default-directory (or directory default-directory)))
        (call-interactively command)))))

(defun shell-command-menu-kill-process ()
  "Kill process of shell command."
  (interactive nil shell-command-menu)
  (cl-loop with row = (tabulated-list-get-id)
           for process in (process-list)
           for item = (process-get process 'shell-command-menu-item)
           when (eq row item)
           return (kill-process process)
           finally do
           (with-slots (name end-time) row
             (setf end-time (time-to-seconds))
             (user-error "No associated process found for `%s'" name))))

(defun shell-command-menu-open ()
  "Switch to active buffer or logs of command."
  (interactive nil shell-command-menu)
  (condition-case err
      (shell-command-menu-switch-to-buffer)
    (user-error (shell-command-menu-find-output))))

(defun shell-command-menu-switch-to-buffer ()
  "Switch to buffer of shell command."
  (interactive nil shell-command-menu)
  (cl-loop with row = (tabulated-list-get-id)
           for process in (process-list)
           for item = (process-get process 'shell-command-menu-item)
           for buffer = (process-buffer process)
           when (and buffer (eq row item))
           return (switch-to-buffer buffer)
           finally (user-error "No associated buffer found for `%s'"
                               (shell-command-menu--item-name row))))

(defun shell-command-menu-find-output ()
  "Open output file of COMMAND."
  (interactive nil shell-command-menu)
  (find-file-read-only (shell-command-menu--file-name (tabulated-list-get-id))))

(defun shell-command-menu-filter-by-command (regex)
  "Filter shell command list by REGEX match of command."
  (interactive (list (read-regexp "Command name" (grep-tag-default))))
  (shell-command-menu t)
  (if (string-empty-p regex)
      (setq shell-command-menu--filter-alist
            (assq-delete-all 'command shell-command-menu--filter-alist))
    (setf (alist-get 'command shell-command-menu--filter-alist)
          `(,(format "Command [%s]" regex)
            ,(lambda (item)
               (with-slots (name) item
                 (string-match-p regex name))))))
  (revert-buffer))

(defun shell-command-menu-filter-by-directory (directory)
  "Filter shell command list by DIRECTORY."
  (interactive "D" shell-command-menu)
  (shell-command-menu t)
  (setf (alist-get 'directory shell-command-menu--filter-alist)
        `(,(format "Directory [%s]"
                   (file-name-nondirectory (directory-file-name directory)))
          ,(let ((directory (abbreviate-file-name directory)))
             (lambda (item)
               (string-prefix-p directory
                                (with-slots (directory) item
                                  (if (file-remote-p directory)
                                      directory
                                    (abbreviate-file-name directory))))))))
  (setq default-directory directory)
  (revert-buffer))

(defun shell-command-menu-filter-by-live ()
  "Filter shell command list by live process."
  (interactive)
  (shell-command-menu t)
  (if (alist-get 'live shell-command-menu--filter-alist)
      (setq shell-command-menu--filter-alist
            (assq-delete-all 'live shell-command-menu--filter-alist))
    (setf (alist-get 'live shell-command-menu--filter-alist)
          `("Live"
            ,(lambda (item)
               (with-slots (end-time) item (not end-time))))))
  (revert-buffer))


;;; Integration's

(with-eval-after-load 'savehist
  (add-to-list 'savehist-minibuffer-history-variables
               'shell-command-menu-items)
  ;; XXX: Make sure that commands from disk have recorded end time.
  (cl-loop for item in shell-command-menu-items do
           (setf (shell-command-menu--item-end-time item)
                 (or (shell-command-menu--item-end-time item)
                     (time-to-seconds)))))

(with-eval-after-load 'evil
  (evil-make-overriding-map shell-command-menu-mode-map))

(defun async-shell-command--fix-tramp (process)
  ;; TODO: Should upstream, this is clearly an oversight
  (when (processp process)
    (with-current-buffer (process-buffer process)
      (shell-command-mode))))

(advice-add #'tramp-handle-shell-command
            :filter-return #'async-shell-command--fix-tramp)

;;; Mode

(defvar shell-command-menu--filter-alist nil)

(defun shell-command-menu--refresh ()
  (setq tabulated-list-format [("" 1 t)
                               ("Directory" 33 t)
                               ("When"  12 t :right-align t)
                               ("Time" 7 t :right-align t)
                               ("Stat" 4 t :right-align t)
                               ("Command" 0 t)])
  (setq tabulated-list-entries nil)
  (cl-flet ((format-time-diff (diff)
              (cond ((> diff (* 24 60 60)) (format-seconds "%dd %hh" diff))
                    ((> diff (* 60 60)) (format-seconds "%hh %mm" diff))
                    (t (format-seconds "%mm %ss%z" diff)))))
    (dolist (item shell-command-menu-items)
      (when (or (not shell-command-menu--filter-alist)
                (cl-loop for (_ _ f) in shell-command-menu--filter-alist
                         unless (funcall f item) return nil
                         finally return t))
        (with-slots (name directory start-time end-time exit-status) item
          (let ((age (format-time-diff
                      (- (or end-time (time-to-seconds)) start-time)))
                (exit-code (cond (exit-status (format "%d" exit-status))
                                 (end-time "??")
                                 ("--")))
                (since
                 (let ((seconds (- (time-to-seconds) start-time)))
                   (if (> seconds (* 7 24 60 60))
                       (format-time-string "%b %d %R" start-time)
                     (format "%s ago" (format-time-diff seconds))))))
            (push `(,item
                    [""
                     ,(string-truncate-left directory 33)
                     ,since
                     ,age
                     ,exit-code
                     ,(propertize name 'face 'proced-executable)])
                  tabulated-list-entries))))))
  (setq tabulated-list-entries (nreverse tabulated-list-entries))
  (tabulated-list-init-header))

(defvar shell-command-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" #'shell-command-menu-open)
    (define-key map "r"  #'shell-command-menu-run)
    (define-key map "e"  #'shell-command-menu-edit)
    (define-key map "x"  #'shell-command-menu-kill-process)
    (define-key map "b"  #'shell-command-menu-switch-to-buffer)
    (define-key map "f"  #'shell-command-menu-find-output)
    (define-key map "s"  #'shell-command-menu-filter-by-command)
    (define-key map "d"  #'shell-command-menu-filter-by-directory)
    (define-key map "a"  #'shell-command-menu-filter-by-live)
    map))

(define-derived-mode shell-command-menu-mode tabulated-list-mode "Shell Command Menu"
  "Major mode for listing the shell commands spawned by Emacs."
  :interactive nil
  (add-hook 'tabulated-list-revert-hook #'shell-command-menu--refresh nil t)
  (setq mode-line-process
        '(shell-command-menu--filter-alist
          (" by "
           (:eval
            (mapconcat #'identity (mapcar #'cadr shell-command-menu--filter-alist)
                       " and "))))))

;;;###autoload
(defun shell-command-menu (display &optional reset)
  "Display shell commands spawned by Emacs.
If DISPLAY is nil do not display created buffer.
If RESET is non nil reset filter state."
  (interactive (list t t))
  (let ((directory default-directory))
    (with-current-buffer (get-buffer-create "*Shell Command List*")
      (shell-command-menu-mode)
      (setq default-directory directory)
      (when reset
        (setq shell-command-menu--filter-alist nil))
      (revert-buffer)
      (when display
        (select-window (display-buffer (current-buffer)))
        (goto-char (point-min))))))

;;;###autoload
(define-minor-mode shell-command-menu-monitor-mode
  "Hook up hooks for monitoring shell-commands."
  :global t
  (cl-loop for (_ _ hook) in shell-command-menu-modes do
           (funcall (if shell-command-menu-monitor-mode #'add-hook #'remove-hook)
                    hook #'shell-command-menu--hook)))

(provide 'shell-command-menu)
;;; shell-command-menu.el ends here

;; Local Variables:
;; checkdoc-force-docstrings-flag: nil
;; End:
