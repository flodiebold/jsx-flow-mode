;;; jsx-flow-mode.el --- Support for JavaScript with JSX and flow annotations -*- lexical-binding: t -*-

;; Version: 0.0.0

;; Author: Florian Diebold <flodiebold@gmail.com>

;; This file is not part of GNU Emacs.

(require 'json)
(require 'js)

(defgroup jsx-flow-faces nil
  "Faces for jsx-flow-mode."
  :group 'faces)

(defface jsx-flow-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for keywords."
  :group 'jsx-flow-faces)

(defface jsx-flow-jsx-tag-face
  '((t :inherit web-mode-html-tag-face))
  "Face for JSX tags."
  :group 'jsx-flow-faces)

(defface jsx-flow-jsx-tag-bracket-face
  '((t :inherit web-mode-html-tag-bracket-face))
  "Face for JSX tags brackets (i.e. < and />)."
  :group 'jsx-flow-faces)

(defconst jsx-flow--keyword-re
  (js--regexp-opt-symbol
   '("break" "case" "catch" "class" "const"
     "continue" "debugger" "default" "delete" "do" "else"
     "export" "extends" "finally" "for"
     "function" "if" "import" "in"
     "instanceof" "new"
     "return" "super" "switch" "throw"
     "this" "try" "typeof" "var" "void"
     "while" "with" "yield"

     ;; es6 strict mode reserved
     "implements" "package" "protected"
     "static" "let" "interface" "private" "public"

     ;; always reserved
     "enum"

     ;; module code reserved
     "await"

     ;; flow
     "type"))
  "Regexp matching any JavaScript (and Flow) keyword.")

(defconst jsx-flow--basic-type-re
  (js--regexp-opt-symbol
   '("boolean" "number" "string" "any" "void"))
  "Regular expression matching any predefined type in Flow.")

(defconst jsx-flow--constant-re
  (js--regexp-opt-symbol '("false" "null" "undefined"
                           "Infinity" "NaN"
                           "true" "arguments" "this"))
  "Regular expression matching any future reserved words in JavaScript.")

(defconst jsx-flow--opening-element-re
  "\\(<\\)\\(\\sw+\\)\\s *\\(?:\\(>\\)\\|\\sw+=\\)")

(defconst jsx-flow--selfclosing-element-re
  "\\(<\\)\\(\\sw+\\)\\s *\\(/>\\)")

(defconst jsx-flow--closing-element-re
  "\\(</\\)\\(\\sw+\\)\\(>\\)")

(defconst jsx-flow--selfclosing-bracket-re
  "\\(/>\\)")

(defconst jsx-flow--font-lock-keywords-2
  `(
    (,jsx-flow--keyword-re . 'font-lock-keyword-face)
    (,jsx-flow--basic-type-re . 'font-lock-type-face)
    (,jsx-flow--constant-re . 'font-lock-constant-face)

    (,jsx-flow--opening-element-re
     (1 'jsx-flow-jsx-tag-bracket-face)
     (2 'jsx-flow-jsx-tag-face)
     (3 'jsx-flow-jsx-tag-bracket-face 'keep t))
    (,jsx-flow--selfclosing-element-re
     (1 'jsx-flow-jsx-tag-bracket-face)
     (2 'jsx-flow-jsx-tag-face)
     (3 'jsx-flow-jsx-tag-bracket-face))
    (,jsx-flow--selfclosing-bracket-re
     (1 'jsx-flow-jsx-tag-bracket-face))
    (,jsx-flow--closing-element-re
     (1 'jsx-flow-jsx-tag-bracket-face)
     (2 'jsx-flow-jsx-tag-face)
     (3 'jsx-flow-jsx-tag-bracket-face))
    ,@js--font-lock-keywords-1
    )
  "Level two font lock keywords for `js-mode'.")

(defconst jsx-flow-mode-syntax-table js-mode-syntax-table)

;;;###autoload
(define-derived-mode jsx-flow-mode
  prog-mode "JSX/Flow"
  "Major mode for JavaScript with Flow and JSX."
  ;; (with-silent-modifications
  ;;   (set-text-properties (point-min) (point-max) nil))
  (setq font-lock-defaults '((jsx-flow--font-lock-keywords-2)))
  )

(provide 'jsx-flow-mode)
;;; jsx-flow-mode.el ends here
