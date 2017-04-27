;;; jest.el --- Jest interface for Emacs -*- lexical-binding: t -*-

;; Author: Florian Diebold <flodiebold@gmail.com>

;; This file is not part of GNU Emacs.

;; Code:

(require 'json)
(require 'dash)
(require 's)
(require 'ht)

(require 'dom)

(defvar jest--servers (ht-create))

(defvar-local jest--root nil)

(defun jest//find-server-script ()
  ;; FIXME
  "/home/florian/Projekte/jsx-flow-mode/jest-server.js")

(defun jest//server-process-filter (process output)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert output)
    (goto-char (point-min))
    (while (looking-at "\\([^\n]*\\)\n")
      (let ((msg-string (match-string-no-properties 1)))
        (delete-region (match-beginning 0) (match-end 0))
        (let ((inhibit-message t))
          (message "jest message: %s" msg-string))
        (with-demoted-errors "jest response handler error: %s"
          (let ((data (json-read-from-string msg-string)))
            (run-hook-with-args 'jest-server-message-hook process data)))))))

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
    (with-current-buffer buf
      (setq jest--root root))
    (with-current-buffer stderr
      (setq jest--root root))
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

(defun jest/run-current-test-file ()
  (interactive)
  (jest//run-test-files (list (buffer-file-name)))
  (jest//display-results-buffer))

(defun jest//display-results-buffer ()
  (let ((server (jest//get-server)))
    (switch-to-buffer-other-window (jest//get-test-results-buffer server))))

(defun jest/get-event (data)
  (alist-get 'event data))

(defun jest//server-root (process)
  (process-get process 'root))

(define-derived-mode jest-results-mode special-mode "Jest results"
  "Mode for the jest results buffer."
  (buffer-disable-undo)
  (setq buffer-read-only t)
  (setq list-buffers-directory (abbreviate-file-name default-directory))
  (when (bound-and-true-p global-linum-mode)
    (linum-mode -1))
  (when (and (fboundp 'nlinum-mode)
             (bound-and-true-p global-nlinum-mode))
    (nlinum-mode -1)))

(defun jest//create-test-results-buffer (root)
  (with-current-buffer (generate-new-buffer "*jest results*")
    (jest-results-mode)
    (current-buffer)))

(defun jest//get-test-results-buffer (process)
  (or (when-let ((buffer (process-get process 'test-results-buffer)))
        (when (buffer-live-p buffer)
          buffer))
      (let* ((root (jest//server-root process))
             (buffer (jest//create-test-results-buffer root)))
        (process-put process 'test-results-buffer buffer)
        buffer)))

(defun jest//handle-pong (server data)
  (when (string-equal (jest/get-event data) "pong")
    (message "got ponged!!")))

(defvar-local jest--test-results nil)

(defun jest//handle-test-result (server data)
  (when (string-equal (jest/get-event data) "testResult")
    (let* ((test-file-path (alist-get 'testFilePath data))
           (test-results (alist-get 'testResults data))
           (test-buffer (jest//get-test-results-buffer server))
           (transformed (jest//transform-test-results data)))
      (with-current-buffer test-buffer
        (unless jest--test-results (setq jest--test-results (ht-create)))
        (--each transformed (jest//merge-test-result it))
        (jest//render-test-results-buffer)))))

(defvar jest-server-message-hook (list #'jest//handle-pong #'jest//handle-test-result))

;; test result transformation
(cl-defstruct jest-test-result
  path ancestor-titles title status failure-messages)

(defun vector-to-list (v)
  (append v nil))

(defun jest//transform-test-result (path data)
  (make-jest-test-result
   :path path
   :ancestor-titles (vector-to-list (alist-get 'ancestorTitles data))
   :title (alist-get 'title data)
   :status (intern (alist-get 'status data))
   :failure-messages (vector-to-list (alist-get 'failureMessages data))))

(defun jest//transform-test-results (data)
  (let ((path (alist-get 'testFilePath data))
        (test-results (alist-get 'testResults data)))
    (--map (jest//transform-test-result path it) test-results)))

(defun jest//merge-test-result (result)
  (let ((suite-name (car (jest-test-result-ancestor-titles result))))
    (unless (ht-contains? jest--test-results suite-name)
      (ht-set! jest--test-results
               ;; TODO: is there always an ancestor title?
               suite-name
               (ht-create)))
    (let ((results (ht-get jest--test-results suite-name)))
      (ht-set! results
               (jest-test-result-title result)
               result))))

(defun jest//aggregate-status (tests)
  (--group-by (jest-test-result-status it) tests))

;; test result rendering
(defun jest//render-test-results-buffer ()
  (save-excursion
    (let ((inhibit-read-only t))
      (let ((node (jest//render-test-results jest--test-results)))
        (goto-char (point-min))
        (dom/mount node)))))

(defun jest//render-test-results (data)
  (let ((suites (sort (ht-keys data) #'string<)))
    (dom/make-node
     :children
     (--map (dom/make-node
             :key it
             :children
             (list
              (jest//render-suite-result
               "" it
               (ht-get data it)))) suites))))

(defun jest//render-suite-result (indent suite-name suite)
  ;; TODO: would need to take nested suites into account for sorting
  (let ((tests (sort (ht-keys suite) #'string<)))
    (dom/node
     suite-name
     " "
     (jest//render-suite-status suite)
     "\n"
     (dom/make-node
      :children
      (--map (dom/make-node
              :key it
              :children
              (list
               (jest//render-test-result
                (concat indent "  ")
                (ht-get suite it)))) tests)))))

(defun jest//render-suite-status (suite)
  (let* ((status (jest//aggregate-status (ht-values suite)))
         (passed (length (alist-get 'passed status)))
         (failed (length (alist-get 'failed status)))
         (pending (length (alist-get 'pending status))))
    (dom/node
     "["
     (propertize (format "%s" passed) 'face '(:foreground "green"))
     (when (< 0 failed)
       (dom/node
        "/"
        (propertize (format "%s" failed) 'face '(:foreground "red"))))
     (when (< 0 pending)
       (dom/node
        "/"
        (propertize (format "%s" pending) 'face '(:foreground "yellow"))))
     "]")))

(defun jest//render-test-result (indent result)
  (dom/node
   (concat indent "  ")
   (case (jest-test-result-status result)
     ('passed (propertize "✓" 'face '(:foreground "green")))
     ('failed (propertize "✕" 'face '(:foreground "red")))
     ('pending (propertize "○" 'face '(:foreground "yellow")))
     (t (propertize "?" 'face 'flycheck-fringe-info)))
   " "
   (dom/make-node
    :children
    (--map (dom/node it " ")
           (cdr (jest-test-result-ancestor-titles result))))
   (dom/with-properties
    (jest-test-result-title result)
    'test-result result)
   "\n"))


;; sections
(cl-defstruct jest-section
  type value start content end hidden)

(defun jest/current-section ()
  (get-text-property (point) 'jest-section))


(provide 'jest)
;; jest.el ends here
