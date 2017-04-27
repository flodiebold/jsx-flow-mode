;;; dom.el --- Virtual DOM for rendering of special buffers -*- lexical-binding: t -*-

;; Author: Florian Diebold <flodiebold@gmail.com>

;; This file is not part of GNU Emacs.

;; Code:

(require 'dash)

(cl-defstruct dom-node
  children properties key (child-key-comparator #'dom//default-key-comparator))

(cl-defstruct dom-mounted-node
  children properties beg end key)

(defun dom/mount (node)
  "Mounts NODE at point. Does not move point."
  (let ((root (dom//ensure-root-at-point)))
    (save-excursion
      (dom//mount-into node root))
    (when (< (dom-mounted-node-beg root)
             (dom-mounted-node-end root))
      (put-text-property (dom-mounted-node-beg root)
                         (1+ (dom-mounted-node-beg root))
                         'dom-root root))))

(defun dom//mounted-text (mounted-node)
  (and (stringp (dom-mounted-node-children mounted-node))
       (dom-mounted-node-children mounted-node)))

(defun dom//mount-list (nodes mounted-nodes key-comparator)
  (dom//sorted-list-merge!
   #'dom//mount-into
   (lambda (node mounted-node)
     (funcall key-comparator (dom//node-key node) (dom-mounted-node-key mounted-node)))
   (lambda (node) (dom//make-empty-node))
   nodes mounted-nodes))

(defun dom//mount-into (node mounted-node)
  (cl-typecase node
    (null
     (dom//delete-node-contents mounted-node)
     (setf (dom-mounted-node-children mounted-node) nil)
     (setf (dom-mounted-node-beg mounted-node) (point))
     (setf (dom-mounted-node-end mounted-node) (point)))
    (string
     (let ((old-length (dom//node-length mounted-node)))
       (setf (dom-mounted-node-beg mounted-node) (point))
       (if (equal-including-properties (dom//mounted-text mounted-node) node)
           (forward-char (length node))
         (delete-char old-length)
         (insert node)
         (setf (dom-mounted-node-children mounted-node) node))
       (setf (dom-mounted-node-end mounted-node) (point))))
    (dom-node
     (setf (dom-mounted-node-key mounted-node) (dom-node-key node))
     (when (dom//mounted-text mounted-node)
       (delete-char (length (dom//mounted-text mounted-node)))
       (setf (dom-mounted-node-children mounted-node) nil))
     (setf (dom-mounted-node-beg mounted-node) (point))
     (setf (dom-mounted-node-children mounted-node)
           (dom//mount-list (dom-node-children node)
                            (dom-mounted-node-children mounted-node)
                            (dom-node-child-key-comparator node)))
     (setf (dom-mounted-node-end mounted-node) (point))))
  (dom//correct-properties node mounted-node)
  (goto-char (dom-mounted-node-end mounted-node)))

(defun dom//node-key (node)
  (cl-typecase node
    (string nil)
    (dom-node (dom-node-key node))))

(defun dom//default-key-comparator (key-a key-b)
  (cond
   ((null key-b) :merge)
   ((equal key-a key-b) :merge)
   ((null key-a) :a)
   ((string< key-a key-b) :a)
   (t :b)))

(defun dom//correct-properties (node mounted-node)
  (setf (dom-mounted-node-properties mounted-node)
        (dom//sorted-alist-reconcile!
         (lambda (node-prop mounted-prop)
           (unless (equal (cdr node-prop) (cdr mounted-prop))
             (if (cdr node-prop)
                 (progn
                   (put-text-property (dom-mounted-node-beg mounted-node)
                                      (dom-mounted-node-end mounted-node)
                                      (car node-prop)
                                      (cdr node-prop))
                   (setcdr mounted-prop (cdr node-prop)))
               (remove-text-properties (dom-mounted-node-beg mounted-node)
                                       (dom-mounted-node-end mounted-node)
                                       (list (car mounted-prop))))))
         #'dom//prop-comparator
         (cl-typecase node
           (string nil)
           (dom-node (dom-node-properties node)))
         (dom-mounted-node-properties mounted-node))))

(defun dom//sorted-list-merge! (merger comparator constructor a b)
  "comparator should return :a, :b or :merge."
  (let* ((prev-link (cons nil b))
         (first-link prev-link))
    (while (and a b)
      (pcase (funcall comparator (car a) (car b))
       (`:merge
        (funcall merger (car a) (car b))
        (setq prev-link b)
        (setq a (cdr a))
        (setq b (cdr b)))
       (`:a
        ;; missing in b
        (setcdr prev-link (cons (funcall constructor (car a)) b))
        (funcall merger (car a) (cadr prev-link))
        (setq prev-link (cdr prev-link))
        (setq a (cdr a)))
       (`:b
        ;; missing in a
        (setcdr prev-link (cdr b))
        (funcall merger nil (car b))
        (setq prev-link b)
        (setq b (cdr b)))
       (other
        (error "wrong return value from comparator func: %s" other))))
    (while a
      (setcdr prev-link (cons (funcall constructor (car a)) nil))
      (funcall merger (car a) (cadr prev-link))
      (setq prev-link (cdr prev-link))
      (setq a (cdr a)))
    (setcdr prev-link nil) ;; remove rest of b
    (while b
      (funcall merger nil (car b))
      (setq b (cdr b)))
    (cdr first-link)))

(defun dom//sorted-alist-reconcile! (fun comparator a b)
  (dom//sorted-list-merge!
   fun (lambda (aa bb)
         (cond
          ((equal (car aa) (car bb)) :merge)
          ((funcall comparator (car aa) (car bb)) :a)
          (t :b)))
   (lambda (aa) (cons (car aa) nil))
   a b))

(defun dom//delete-node-contents (node)
  (delete-char (dom//node-length node)))

(defun dom//make-empty-node ()
  (make-dom-mounted-node :beg (point-min) :end (point-min)))

(defun dom//ensure-root-at-point ()
  (or (dom//root-at-point)
      (dom//make-empty-node)))

(defun dom//node-length (mounted-node)
  (- (dom-mounted-node-end mounted-node) (dom-mounted-node-beg mounted-node)))

(defun dom//root-at-point ()
  (get-text-property (point) 'dom-root))

(defun dom/node (&rest children)
  (make-dom-node :children children))

(defun dom/make-node (&rest args)
  (apply #'make-dom-node args))

(defun plist->alist (plist)
  (let ((alist nil))
    (while plist
      (setq alist (cons (cons (car plist) (cadr plist)) alist))
      (setq plist (cddr plist)))
    alist))

(defun dom//prop-comparator (a b)
  (string< (symbol-name a) (symbol-name b)))

(defun dom//sort-properties (alist)
  (sort alist #'dom//prop-comparator))

(defun dom/with-properties (child &rest properties)
  (make-dom-node :children (list child)
                 :properties (dom//sort-properties (plist->alist properties))))


(provide 'dom)
;; dom.el ends here
