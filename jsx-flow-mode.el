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

(defface jsx-flow-jsx-attribute-face
  '((t :inherit web-mode-html-attr-name-face))
  "Face for JSX attributes."
  :group 'jsx-flow-faces)


;; flow stuff
(defun jsx-flow//column-number-at-pos (pos)
  "column number at pos"
  (save-excursion (goto-char pos) (current-column)))

(defmacro jsx-flow|measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let* ((time (current-time))
          (result ,@body))
     (message "%.06f" (float-time (time-since time)))
     result))

(defun jsx-flow//call-flow-into-buffer (&rest args)
  "Calls flow with args on the current buffer, returns the result."
  (jsx-flow|measure-time
   (let ((buf (generate-new-buffer jsx-flow:buffer-name)))
     (apply 'call-process-region (point-min) (point-max) "flow" nil buf nil args)
     buf)))

(defun jsx-flow//call-flow-on-current-buffer (&rest args)
  "Calls flow with args on the current buffer, returns the result."
  (jsx-flow|measure-time
   (let* ((buf (generate-new-buffer "*flow*")))
     (unwind-protect
         (let* ((result (apply 'call-process-region (point-min) (point-max) "flow" nil buf nil args))
                (output (with-current-buffer buf (buffer-string))))
           (when (= result 0)
             output))
       (kill-buffer buf)))))

(defun jsx-flow//call-flow-on-current-buffer-async (result-handler &rest args)
  "Calls flow with args on the current buffer asynchronously; passes the result to result-handler."
  (let* ((buf (generate-new-buffer "*flow*"))
         (process (apply #'start-process "flow" buf "flow" args)))
    (set-process-sentinel process
                          (lambda (process event)
                            (when (equal 'exit (process-status process))
                              (let ((output (with-current-buffer (process-buffer process) (buffer-string))))
                                (kill-buffer (process-buffer process))
                                (with-demoted-errors "jsx-flow: error in flow result handler: %s"
                                  (funcall result-handler output))))))
    (when (process-live-p process)
      (with-demoted-errors "jsx-flow: error calling flow: %s"
        (process-send-region process (point-min) (point-max))
        (process-send-eof process)))))

(defun jsx-flow//json-flow-call (&rest args)
  "Calls flow on the current buffer passing --json, parses the result."
  (let* ((args (append args '("--json")))
         (output (apply #'jsx-flow//call-flow-on-current-buffer args)))
    (when output
      (json-read-from-string output))))

(defun jsx-flow//json-flow-call-async (result-handler &rest args)
  "Calls flow on the current buffer passing --json asynchronously; parses the result and gives it to result-handler."
  (let ((args (append args '("--json")))
        (handler (lambda (output) (funcall result-handler (json-read-from-string output)))))
    (apply #'jsx-flow//call-flow-on-current-buffer-async handler args)))

(defun jsx-flow//pos-to-flow-location (pos)
  "Returns a list of (line col) for pos in the current buffer."
  (let ((line (line-number-at-pos pos))
        (col (1+ (jsx-flow//column-number-at-pos pos))))
    (list (number-to-string line) (number-to-string col))))

(defun jsx-flow//get-def (pos)
  "Calls flow to get the definition location of the thing at pos, returns the result."
  (let* ((loc (jsx-flow//pos-to-flow-location pos))
         (filename (buffer-file-name)))
    (apply #'jsx-flow//json-flow-call "get-def" filename loc)))

(defun jsx-flow//show-flow-loc (loc)
  "Takes a flow location info and shows it."
  (let* ((filename (cdr (assq 'path loc)))
         (line (cdr (assq 'line loc)))
         (col (cdr (assq 'start loc))))
    (when (not (eq filename ""))
      (find-file filename)
      (goto-char (point-min))
      (forward-line (1- line))
      (forward-char (1- col)))))

(defun jsx-flow/get-def-at-point ()
  "Show the definition of the thing at point using flow."
  (interactive)
  (let ((loc (jsx-flow//get-def (point))))
    (jsx-flow//show-flow-loc loc)))

(defun jsx-flow/suggest-into-buffer ()
  "Calls flow suggest and then runs ediff with the result."
  (interactive)
  (let* ((filename (buffer-file-name))
         (diff-buffer (jsx-flow//call-flow-into-buffer "suggest" filename)))
    (ediff-patch-file 2 diff-buffer)))

(defun jsx-flow//type-at-pos-async (result-handler pos)
  "Calls flow to get the type at pos asynchronously; passes the result to result-handler."
  (let* ((loc (jsx-flow//pos-to-flow-location pos))
         (filename (buffer-file-name)))
    (apply #'jsx-flow//json-flow-call-async result-handler "type-at-pos" filename loc)))

(defun jsx-flow//eldoc-show-type-info (data)
  "Shows the passed type info using eldoc."
  (let ((type (cdr (assq 'type data))))
    (when (not (equal "(unknown)" type))
      (eldoc-message type))))

(defun jsx-flow/eldoc-show-type-at-point ()
  "Shows type at point."
  (interactive)
  (jsx-flow//type-at-pos-async #'jsx-flow//eldoc-show-type-info (point))
  nil)

(defvar jsx-flow--ast nil
  "The AST from flow.")

(defun jsx-flow//receive-ast (data)
  "Handler for the flow AST call."
  (message "jsx-flow//receive-ast called")
  (let ((ast (json-read-from-string data)))
    (setq jsx-flow--ast ast)
    ;; (font-lock-fontify-buffer)
    ))

(defun jsx-flow//do-parse ()
  "Calls flow to get the AST and stores it in flowtype--ast."
  (flowtype//call-flow-on-current-buffer-async #'jsx-flow//receive-ast "ast"))

;; AST functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jsx-flow//create-node (type)
  "Creates a new AST node."
  (list* (cons 'type type) (cons 'range (vector (copy-marker (point)) (copy-marker (point)));;(jsx-flow//make-node-range (point) (point))
                                 )
         ;; initialize additional fields
         (case type
           (('Program 'BlockStatement) (list (cons 'body (vector)))))))

(defun jsx-flow//vector-field-p (fieldname)
  (memq fieldname '(typeParameters elements properties expressions arguments params declarations specifiers types properties indexers callProperties)))

(defun jsx-flow//make-node-range (start end)
  "Formats the given range for the range field of a node."
  (vector (copy-marker start t) (copy-marker end nil)))

(defun jsx-flow//node-start (node)
  "Returns the (inclusive) start position of node."
  (let ((pos (elt (cdr (assq 'range node)) 0)))
    (if (markerp pos)
        pos
      (1+ pos))))

(defun jsx-flow//node-end (node)
  "Returns the (exclusive) end position of node."
  (let ((pos (elt (cdr (assq 'range node)) 1)))
    (if (markerp pos)
        pos
      (1+ pos))))

(defun jsx-flow//node-field (node field)
  "Returns the given field of node."
  (cdr (assq field node)))

(defun jsx-flow//node-fields (node)
  "Returns all fields of node."
  (map 'list #'car node))

(defun jsx-flow//set-node-field (node field value)
  "Sets the given field of node"
  (let ((pair (assq field node)))
    (if pair
        (setcdr (assq field node) value)
      (nconc node (list (cons field value))))))

(defun jsx-flow//node-type (node)
  "Returns the node type of node."
  (let ((value (jsx-flow//node-field node 'type)))
    (if (symbolp value)
        value
      (intern value))))

(defun jsx-flow//node-name (node)
  "Returns the name of node (for JSX opening and closing elements)."
  (jsx-flow//node-field node 'name))

(defun jsx-flow//node-body (node)
  "Returns the body of node."
  (jsx-flow//node-field node 'body))

(defun jsx-flow//node-value (node)
  "Returns the value of node."
  (jsx-flow//node-field node 'value))

(defun jsx-flow//node-expression (node)
  "Returns the expression of node."
  (jsx-flow//node-field node 'expression))

(defun jsx-flow//node-left (node)
  "Returns the left of node."
  (jsx-flow//node-field node 'left))

(defun jsx-flow//node-right (node)
  "Returns the right of node."
  (jsx-flow//node-field node 'right))

(defun jsx-flow//node-object (node)
  "Returns the object of node."
  (jsx-flow//node-field node 'object))

(defun jsx-flow//node-property (node)
  "Returns the property of node."
  (jsx-flow//node-field node 'property))

(defun jsx-flow//jsx-opening-element (jsx-element)
  "Returns the opening element of the jsx-element."
  (jsx-flow//node-field jsx-element 'openingElement))

(defun jsx-flow//jsx-closing-element (jsx-element)
  "Returns the closing element of the jsx-element."
  (jsx-flow//node-field jsx-element 'closingElement))

(defun jsx-flow//node-contents (node)
  "Returns the complete text contents of the node."
  (buffer-substring (jsx-flow//node-start node) (jsx-flow//node-end node)))

(defun jsx-flow//body-p (node-type)
  "Non-nil if node-type just consists of its body."
  (memq node-type '(Program BlockStatement ClassBody)))

(defun jsx-flow//leaf-p (node-type)
  "Non-nil if node-type has no children."
  (memq node-type '(NumberTypeAnnotation
                    NumberLiteralTypeAnnotation
                    StringTypeAnnotation
                    StringLiteralTypeAnnotation
                    VoidTypeAnnotation
                    BooleanTypeAnnotation
                    BooleanLiteralTypeAnnotation
                    AnyTypeAnnotation
                    MixedTypeAnnotation

                    EmptyStatement
                    DebuggerStatement

                    Literal
                    ThisExpression
                    RegExpLiteral

                    JSXIdentifier
                    JSXText)))

(defun jsx-flow//visit (fun thing)
  "If thing is a vector, run fun on each element; otherwise run fun on thing."
  (cond
   ((vectorp thing)
    (loop for child being the elements of thing
          do (jsx-flow//visit fun child)))
   ((null thing) nil)
   (t (funcall fun thing))))

(defun jsx-flow//visit-fields (fields fun thing)
  "Visits all fields of thing with fun."
  (dolist (field fields)
    (jsx-flow//visit fun (jsx-flow//node-field thing field))))

(defun jsx-flow//visit-children (fun ast-node)
  "Runs fun on each of ast-node's children in turn."
  (pcase (jsx-flow//node-type ast-node)
    ((pred jsx-flow//leaf-p)
     nil)
    ((pred jsx-flow//body-p)
     (jsx-flow//visit-fields '(body) fun ast-node))

    ;; Expressions
    ((or `AssignmentExpression `BinaryExpression `LogicalExpression)
     (jsx-flow//visit-fields '(left right) fun ast-node))
    (`MemberExpression
     (jsx-flow//visit-fields '(object property) fun ast-node))
    (`FunctionExpression
     (jsx-flow//visit-fields '(id typeParameters params returnType body) fun ast-node))
    (`ArrayExpression
     (jsx-flow//visit-fields '(elements) fun ast-node))
    (`ObjectExpression
     (jsx-flow//visit-fields '(properties) fun ast-node))
    (`Property
     (jsx-flow//visit-fields '(key value) fun ast-node))
    (`SequenceExpression
     (jsx-flow//visit-fields '(expressions) fun ast-node))
    ((or `UpdateExpression `UnaryExpression)
     (jsx-flow//visit-fields '(argument) fun ast-node))
    (`ConditionalExpression
     (jsx-flow//visit-fields '(test alternate consequent) fun ast-node))
    ((or `NewExpression `CallExpression)
     (jsx-flow//visit-fields '(callee arguments) fun ast-node))
    ((or `ClassDeclaration `ClassExpression)
     (jsx-flow//visit-fields '(id superClass body implements typeParameters superTypeParameters) fun ast-node))

    ;; Class declarations
    (`MethodDefinition
     (jsx-flow//visit-fields '(key value decorators) fun ast-node))
    (`ClassProperty
     (jsx-flow//visit-fields '(key value typeAnnotation) fun ast-node))

    ;; Statements
    (`ExpressionStatement
     (jsx-flow//visit-fields '(expression) fun ast-node))
    (`IfStatement
     (jsx-flow//visit-fields '(test consequent alternate) fun ast-node))
    (`LabeledStatement
     (jsx-flow//visit-fields '(label body) fun ast-node))
    ((or `ContinueStatement `BreakStatement)
     (jsx-flow//visit-fields '(label) fun ast-node))
    (`WithStatement
     (jsx-flow//visit-fields '(object body) fun ast-node))
    (`SwitchStatement
     (jsx-flow//visit-fields '(discriminant cases) fun ast-node))
    ((or `ThrowStatement `ReturnStatement)
     (jsx-flow//visit-fields '(argument) fun ast-node))
    (`TryStatement
     (jsx-flow//visit-fields '(block handler finalizer) fun ast-node))
    (`WhileStatement
     (jsx-flow//visit-fields '(test body) fun ast-node))
    (`DoWhileStatement
     (jsx-flow//visit-fields '(body test) fun ast-node))
    (`ForStatement
     (jsx-flow//visit-fields '(init test update body) fun ast-node))
    ((or `ForOfStatement `ForInStatement)
     (jsx-flow//visit-fields '(left right body) fun ast-node))
    (`FunctionDeclaration
     (jsx-flow//visit-fields '(id typeParameters params returnType body) fun ast-node))
    (`VariableDeclaration
     (jsx-flow//visit-fields '(declarations) fun ast-node))
    (`VariableDeclarator
     (jsx-flow//visit-fields '(id init) fun ast-node))

    ;; Clauses
    (`SwitchCase
     (jsx-flow//visit-fields '(test consequent) fun ast-node))
    (`CatchClause
     (jsx-flow//visit-fields '(param body) fun ast-node))

    ;; Top-level declarations
    (`ExportDeclaration
     (jsx-flow//visit-fields '(specifiers declaration) fun ast-node))

    ;; Type annotations
    ((or `TypeAnnotation `Identifier)
     (jsx-flow//visit-fields '(typeAnnotation) fun ast-node))
    (`TypeAlias
     (jsx-flow//visit-fields '(id typeParameters right) fun ast-node))
    ((or `IntersectionTypeAnnotation `UnionTypeAnnotation `TupleTypeAnnotation)
     (jsx-flow//visit-fields '(types) fun ast-node))
    (`TypeofTypeAnnotation
     (jsx-flow//visit-fields '(argument) fun ast-node))
    (`GenericTypeAnnotation
     (jsx-flow//visit-fields '(id typeParameters) fun ast-node))
    (`ArrayTypeAnnotation
     (jsx-flow//visit-fields '(elementType) fun ast-node))
    (`ObjectTypeAnnotation
     (jsx-flow//visit-fields '(properties indexers callProperties) fun ast-node))
    (`ObjectTypeProperty
     (jsx-flow//visit-fields '(key value) fun ast-node))
    (`ObjectTypeIndexer
     (jsx-flow//visit-fields '(id key value) fun ast-node))
    (`ObjectTypeCallProperty
     (jsx-flow//visit-fields '(value) fun ast-node))
    (`FunctionTypeAnnotation
     (jsx-flow//visit-fields '(typeParameters params rest returnType) fun ast-node))
    (`FunctionTypeParam
     (jsx-flow//visit-fields '(name typeAnnotation) fun ast-node))
    (`TypeCastExpression
     (jsx-flow//visit-fields '(expression typeAnnotation) fun ast-node))
    ((or `TypeParameterInstantiation `TypeParameterDeclaration)
     (jsx-flow//visit-fields '(params) fun ast-node))
    (`QualifiedTypeIdentifier
     (jsx-flow//visit-fields '(qualification id) fun ast-node))

    ;; JSX
    (`JSXElement
     (jsx-flow//visit-fields '(openingElement children closingElement) fun ast-node))
    (`JSXOpeningElement
     (jsx-flow//visit-fields '(name attributes) fun ast-node))
    (`JSXClosingElement
     (jsx-flow//visit-fields '(name) fun ast-node))
    (`JSXAttribute
     (jsx-flow//visit-fields '(name value) fun ast-node))
    (`JSXExpressionContainer
     (jsx-flow//visit-fields '(expression) fun ast-node))
    (`JSXSpreadAttribute
     (jsx-flow//visit-fields '(argument) fun ast-node))

    ;; TODO: NullableTypeAnnotation, TemplateLiteral

    (unknown
     (message "Unknown node type: %s %s" unknown (jsx-flow//node-fields ast-node)))))

(defun jsx-flow//jsx-matching-element (jsx-element)
  "Returns the closing element for an opening element and vice versa."
  (let ((parent (jsx-flow//node-parent jsx-element)))
    (case (jsx-flow//node-type jsx-element)
      ('JSXOpeningElement (jsx-flow//jsx-closing-element parent))
      ('JSXClosingElement (jsx-flow//jsx-opening-element parent)))))

(defun jsx-flow//walk-ast-print-types (ast-node)
  "Walks the ast, printing node types."
  (message "Node: %s" (jsx-flow//node-type ast-node))
  (jsx-flow//visit-children #'jsx-flow//walk-ast-print-types ast-node))


;; company provider (TODO)

(defun jsx-flow//fetch-completions (&rest _)
  (interactive "P")
  (let* ((loc (jsx-flow//pos-to-flow-location (point)))
         (filename (buffer-file-name))
         (response (jsx-flow//json-flow-call "autocomplete" (car loc) (cadr loc)))
         (result (cdr (assoc 'result response)))
         (names (mapcar (lambda (res) (cdr (assoc 'name res))) result)))
    names))

(defun company-jsx-flow-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (case command
    (interactive (company-begin-backend 'company-jsx-flow-backend))
    (prefix (and (eq major-mode 'jsx-flow-mode)
                 (company-grab-symbol)))
    (candidates
     (progn
       (message "candidates %s" arg)
       (let* ((completes (jsx-flow//fetch-completions))
              (_ (message "names %s" completes))
              (list (remove-if-not
                    (lambda (c) (string-prefix-p arg c))
                    completes)))
         (message "list %s" list)
         list)))))

;; TODO
;; (add-to-list 'company-backends 'company-jsx-flow-backend)

;; flycheck

(defun jsx-flow//fc-convert-error (error checker counter)
  "Return a list of errors from ERROR."
  (let* ((msg-parts (cdr (assoc 'message error)))
         (first-part (elt msg-parts 0))
         (level (if (eq (cdr (assoc 'level first-part)) "warning") 'warning 'error))
         (file (cdr (assoc 'path first-part)))
         (line (cdr (assoc 'line first-part)))
         (col  (cdr (assoc 'start first-part)))
         (desc (--reduce (format "%s\n%s" acc it) (--map (cdr (assoc 'descr it)) msg-parts))))
    (if (string= file (buffer-file-name))
        (list (flycheck-error-new-at line col level desc :checker checker :id counter))
      '())))

(defun jsx-flow//parse-status-errors (output checker buffer)
  "Parse flow status errors in OUTPUT."
  (let* ((json (json-read-from-string output))
         (errors (cdr (assoc 'errors json)))
         (counter 0)
         (errs (-mapcat
                (lambda (err)
                  (setq counter (1+ counter))
                  (jsx-flow//fc-convert-error err checker (number-to-string counter)))
                errors)))
    (if errs
        errs
      (list (flycheck-error-new-at 0 0 'warning "Errors in other files exist" :checker checker :id "0")))))

(with-eval-after-load 'flycheck
  (flycheck-define-command-checker 'javascript-flow
    "A JavaScript syntax and style checker using Flow."
    :command '("flow" "status" "--json")
    :error-parser #'jsx-flow//parse-status-errors
    :next-checkers '((error . javascript-eslint))
    :modes '(jsx-flow-mode))

  (flycheck-add-mode 'javascript-eslint 'jsx-flow-mode)
  (add-to-list 'flycheck-checkers 'javascript-flow)
  )


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
  "\\(<\\)\\([[:alnum:]]+\\)\\([[:space:]\n]+[[:alnum:]/]\\|>\\)")

(defconst jsx-flow--selfclosing-element-re
  "\\(<\\)\\([[:alnum:]]+\\)[[:space:]\n]*\\(/>\\)")

(defconst jsx-flow--closing-element-re
  "\\(</\\)\\([[:alnum:]]+\\)\\(>\\)")

(defconst jsx-flow--jsx-attribute-re
  "\\([[:alnum:]-]+\\)\\|[{/>]")

(defconst jsx-flow--selfclosing-bracket-re
  "\\(/>\\)")

(defun jsx-flow//find-next-jsx-attribute (limit)
  (let ((result (re-search-forward jsx-flow--jsx-attribute-re limit t)))
    (when result
      (case (char-after (match-beginning 0))
        (123 ;; 123 = {
         (goto-char (scan-sexps (point) 1))
         (jsx-flow//find-next-jsx-attribute limit))
        ((?/ ?>) nil)
        (t
         (jsx-flow//jump-over-jsx-attribute-value limit)
         result)))))

(defun jsx-flow//jump-over-jsx-attribute-value (limit)
  (skip-syntax-forward " ")
  (when (= (following-char) ?=)
    (forward-char 1)
    (skip-syntax-forward " ")
    (goto-char (scan-sexps (point) 1))
    (skip-syntax-forward " ")))

(defun jsx-flow//jump-to-end-of-jsx-tag (limit)
  (let ((found nil))
    (while (and (not found) (< (point) limit))
      (if (re-search-forward "/>\\|[{\"'>]" limit t)
          (pcase (match-string 0)
            ((or "/>" ">") (setq found t))
            (_ (goto-char (scan-sexps (match-beginning 0) 1))))
        ;; not found, go to end
        (goto-char limit)))
    found))

(defun jsx-flow//find-end-of-jsx-tag (limit)
  (save-excursion
    (save-match-data
      (jsx-flow//jump-to-end-of-jsx-tag limit)
      (point))))

(defun jsx-flow//in-jsx-p ())

(defun jsx-flow//font-lock-extend-region ()
  ;; TODO: improve this!
  (unless (= font-lock-beg (point-min))
    (setq font-lock-beg (point-min))
    t))

(defconst jsx-flow--font-lock-keywords-2
  `(
    (,jsx-flow--keyword-re . 'font-lock-keyword-face)
    (,jsx-flow--basic-type-re . 'font-lock-type-face)
    (,jsx-flow--constant-re . 'font-lock-constant-face)

    ;; opening / self-closing jsx elements
    (,jsx-flow--opening-element-re
     (1 'jsx-flow-jsx-tag-bracket-face)
     (2 'jsx-flow-jsx-tag-face)
     (jsx-flow//find-next-jsx-attribute
      ;; jump to beginning of attribute list,
      ;; and allow searching until end of file
      (progn (goto-char (match-end 2)) (point-max))
      (goto-char (match-end 2))
      (0 'jsx-flow-jsx-attribute-face))
     ;; color closing bracket
     (jsx-flow//jump-to-end-of-jsx-tag
      (progn (goto-char (match-end 2)) (point-max))
      (goto-char (match-end 2))
      (0 'jsx-flow-jsx-tag-bracket-face)
      ))
    ;; closing jsx elements
    (,jsx-flow--closing-element-re
     (1 'jsx-flow-jsx-tag-bracket-face)
     (2 'jsx-flow-jsx-tag-face)
     (3 'jsx-flow-jsx-tag-bracket-face))
    ,@js--font-lock-keywords-1
    )
  "Level two font lock keywords for `js-mode'.")

(defconst jsx-flow--font-lock-keywords-ast
  `(,@jsx-flow--font-lock-keywords-2))

(defconst jsx-flow-mode-syntax-table js-mode-syntax-table)

;;;###autoload
(define-derived-mode jsx-flow-mode
  prog-mode "JSX/Flow"
  "Major mode for JavaScript with Flow and JSX."
  (with-silent-modifications
    (set-text-properties (point-min) (point-max) nil))
  (setq font-lock-defaults '((jsx-flow--font-lock-keywords-ast)))
  (setq font-lock-extend-region-functions '(jsx-flow//font-lock-extend-region))
  (set (make-local-variable 'eldoc-documentation-function) #'flowtype/eldoc-show-type-at-point)
  (turn-on-eldoc-mode)
  (flycheck-mode 1))

(provide 'jsx-flow-mode)
;;; jsx-flow-mode.el ends here
