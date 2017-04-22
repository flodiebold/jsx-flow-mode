;;; jest.el --- Jest interface for Emacs -*- lexical-binding: t -*-

;; Author: Florian Diebold <flodiebold@gmail.com>

;; This file is not part of GNU Emacs.

;; Code:

(require 'json)
(require 'dash)
(require 's)

(defvar-local jest--server-process nil)

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
  (message "jest process event: %s" event))

(defun jest//start-server-process (cwd)
  (let* ((cwd (file-truename cwd))
         (process-environment (cons (concat "NODE_PATH=" cwd "node_modules")
                                    process-environment))
         (default-directory cwd)
         (buf (generate-new-buffer "*jest-server*"))
         (stderr (generate-new-buffer "*jest-server-stderr*"))
         (command (list "node" (jest//find-server-script)))
         (process (make-process :name "jest"
                                :buffer buf
                                :stderr stderr
                                :command command)))
    (set-process-filter process #'jest//server-process-filter)
    (set-process-sentinel process #'jest//server-process-sentinel)
    process))

(defun jest//stop-server-process (process)
  ;; TODO
  )

(defun jest//get-server ()
  (or jest--server-process
      (when-let ((package-json-dir (locate-dominating-file (buffer-file-name) "package.json")))
        ;; TODO one jest process per package.json...
        (setq jest--server-process
              (jest//start-server-process package-json-dir)))))

(defun jest//send-message-string (msg)
  (process-send-string (jest//get-server) (concat msg "\n")))

(defun jest//send-message (msg-data)
  (let ((string (json-encode msg-data)))
    (jest//send-message-string string)))

(defun jest//send-ping ()
  (jest//send-message '((command . "ping"))))

(defun jest//run-current-test-file ()
  (jest//send-message `((command . "runTests")
                        (files . [,(file-truename (buffer-file-name))]))))

(defun jest/get-event (data)
  (alist-get 'event data))

(defun jest//handle-pong (data)
  (when (string-equal (jest/get-event data) "pong")
    (message "got ponged!!")))

(defvar jest-server-message-hook (list #'jest//handle-pong))


(provide 'jest)
;; jest.el ends here
