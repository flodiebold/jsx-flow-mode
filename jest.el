;;; jest.el --- Jest interface for Emacs -*- lexical-binding: t -*-

;; Author: Florian Diebold <flodiebold@gmail.com>

;; This file is not part of GNU Emacs.

;; Code:

(require 'json)
(require 'dash)
(require 's)
(require 'ht)

(defvar jest--servers (ht-create))

(defvar-local jest--root nil)

(defun jest//find-server-script ()
  ;; FIXME
  "/home/florian/Projekte/privat/jsx-flow-mode/jest-server.js")

(defun jest//server-process-filter (process output)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert output)
    (goto-char (point-min))
    (while (looking-at "\\([^\n]*\\)\n")
      (let ((msg-string (match-string-no-properties 1)))
        (delete-region (match-beginning 0) (match-end 0))
        (message "jest message: %s" msg-string)
        (with-demoted-errors "jest response handler error: %s"
          (let ((data (json-read-from-string msg-string)))
            (run-hook-with-args 'jest-server-message-hook data)))))))

(defun jest//server-process-sentinel (process event)
  (unless (process-live-p process)
    ;; cleanup
    (let ((root (process-get process 'root))
          (stderr (process-get process 'stderr)))
      (ht-remove! jest--servers root)
      (kill-buffer (process-buffer process))
      (when (or (zerop (process-exit-status process))
                (process-get process 'killed-from-emacs))
        ;; don't clean up stderr buffer if the process crashed
        (kill-buffer stderr)))))

(defun jest//start-server-process (root)
  (let* ((process-environment (cons (concat "NODE_PATH=" root "/node_modules")
                                    process-environment))
         (default-directory root)
         (buf (generate-new-buffer "*jest-server*"))
         (stderr (generate-new-buffer "*jest-server-stderr*"))
         (stderr-pipe (make-pipe-process :name "jest-error" :buffer stderr
                                         :noquery t))
         (command (list "node" (jest//find-server-script)))
         (process (make-process :name "jest"
                                :buffer buf
                                :stderr stderr
                                :command command)))
    (set-process-filter process #'jest//server-process-filter)
    (set-process-sentinel process #'jest//server-process-sentinel)
    (process-put process 'stderr stderr)
    (process-put process 'root root)
    process))

(defun jest//get-root ()
  (or jest--root
      (progn
        (setq jest--root
              (file-truename (directory-file-name (locate-dominating-file (buffer-file-name) "package.json"))))
        jest--root)))

(defun jest//get-server ()
  (when-let ((root (jest//get-root)))
    (or (ht-get jest--servers root)
        (let ((server (jest//start-server-process root)))
          (ht-set! jest--servers root server)
          server))))

(defun jest/kill-server ()
  (interactive)
  (when-let ((server (ht-get jest--servers (jest//get-root))))
    (process-put server 'killed-from-emacs t)
    (kill-process server)))

(defun jest//send-message-string (msg)
  (process-send-string (jest//get-server) (concat msg "\n")))

(defun jest//send-message (msg-data)
  (let ((string (json-encode msg-data)))
    (jest//send-message-string string)))

(defun jest//send-ping ()
  (jest//send-message '((command . "ping"))))

(defun jest//run-test-files (files)
  (jest//send-message `((command . "runTests")
                        (files . ,(vconcat (--map (file-truename it) files))))))

(defun jest//run-current-test-file ()
  (jest//run-test-files (list (buffer-file-name))))

(defun jest/get-event (data)
  (alist-get 'event data))

(defun jest//handle-pong (data)
  (when (string-equal (jest/get-event data) "pong")
    (message "got ponged!!")))

(defvar jest-server-message-hook (list #'jest//handle-pong))


(provide 'jest)
;; jest.el ends here
