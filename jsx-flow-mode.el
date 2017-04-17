;;; jsx-flow-mode.el --- Support for JavaScript with JSX and flow annotations -*- lexical-binding: t -*-

;; Version: 0.1.0

;; Author: Florian Diebold <flodiebold@gmail.com>

;; This file is not part of GNU Emacs.

(require 'json)
(require 'js)
(require 'dash)

(defgroup jsx-flow-faces nil
  "Faces for jsx-flow-mode."
  :group 'faces)

(defface jsx-flow-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for keywords."
  :group 'jsx-flow-faces)

(defface jsx-flow-jsx-tag-face
  '((t :weight normal :inherit font-lock-keyword-face))
  "Face for JSX tags."
  :group 'jsx-flow-faces)

(defface jsx-flow-jsx-tag-bracket-face
  '((t :inherit shadow))
  "Face for JSX tags brackets (i.e. < and />)."
  :group 'jsx-flow-faces)

(defface jsx-flow-jsx-attribute-face
  '((t :weight normal :slant italic :inherit font-lock-constant-face))
  "Face for JSX attributes."
  :group 'jsx-flow-faces)

(defface jsx-flow-untyped-face
  '((t :inherit diff-refine-removed))
  "Face used to highlight untyped expressions."
  :group 'jsx-flow-faces)

;; utils
(defun assq-recursive (alist &rest keys)
  "Recursively find KEYs in ALIST."
  (while keys
    (setq alist (cdr (assq (pop keys) alist))))
  alist)


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

(defun jsx-flow//call-flow-on-current-buffer (&rest args)
  "Calls flow with args on the current buffer, returns the result."
  (let* ((buf (generate-new-buffer "*flow*")))
    (unwind-protect
        (let* ((result (apply 'call-process-region (point-min) (point-max) "flow" nil buf nil args))
               (output (with-current-buffer buf (buffer-string))))
          (when (= result 0)
            output))
      (kill-buffer buf))))

(defun jsx-flow//call-flow-on-current-buffer-async (result-handler &rest args)
  "Calls flow with args on the current buffer asynchronously; passes the result to result-handler."
  (let* ((buf (generate-new-buffer "*flow*"))
         (process (apply #'start-process "flow" buf "flow" args)))
    (set-process-sentinel process
                          (lambda (process _event)
                            (when (equal 'exit (process-status process))
                              (let ((output (with-current-buffer (process-buffer process) (buffer-string))))
                                (kill-buffer (process-buffer process))
                                (with-demoted-errors "jsx-flow: error in flow result handler: %s"
                                  (funcall result-handler output))))))
    (when (process-live-p process)
      (with-demoted-errors "jsx-flow: error calling flow: %s"
        (process-send-region process (point-min) (point-max))
        (process-send-eof process)))))

(defun jsx-flow//call-flow-async (result-handler &rest args)
  "Calls flow with args asynchronously; passes the result to result-handler."
  (let* ((buf (generate-new-buffer "*flow*"))
         (process (apply #'start-process "flow" buf "flow" args)))
    (set-process-sentinel process
                          (lambda (process _event)
                            (when (equal 'exit (process-status process))
                              (let ((output (with-current-buffer (process-buffer process) (buffer-string))))
                                (kill-buffer (process-buffer process))
                                (with-demoted-errors "jsx-flow: error in flow result handler: %s"
                                  (funcall result-handler output))))))))

(defun jsx-flow//json-call-flow-on-current-buffer (&rest args)
  "Calls flow on the current buffer passing --json, parses the result."
  (let* ((args (append args '("--json")))
         (output (apply #'jsx-flow//call-flow-on-current-buffer args)))
    (when output
      (json-read-from-string output))))

(defun jsx-flow//json-call-flow-on-current-buffer-async (result-handler &rest args)
  "Calls flow on the current buffer passing --json asynchronously; parses the result and gives it to result-handler."
  (let ((args (append args '("--json")))
        (handler (lambda (output) (funcall result-handler (json-read-from-string output)))))
    (apply #'jsx-flow//call-flow-on-current-buffer-async handler args)))

(defun jsx-flow//json-call-flow-async (result-handler &rest args)
  "Calls flow, passing --json asynchronously; parses the result and gives it to result-handler."
  (let ((args (append args '("--json")))
        (handler (lambda (output) (funcall result-handler (json-read-from-string output)))))
    (apply #'jsx-flow//call-flow-async handler args)))

(defun jsx-flow//pos-to-flow-location (pos)
  "Returns a list of (line col) for pos in the current buffer."
  (let ((line (line-number-at-pos pos))
        (col (1+ (jsx-flow//column-number-at-pos pos))))
    (list (number-to-string line) (number-to-string col))))

(defun jsx-flow//get-def (pos)
  "Calls flow to get the definition location of the thing at pos, returns the result."
  (let* ((loc (jsx-flow//pos-to-flow-location pos))
         (filename (buffer-file-name)))
    (apply #'jsx-flow//json-call-flow-on-current-buffer "get-def" filename loc)))

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

(defun jsx-flow//find-refs (pos)
  "Calls flow to get all refs of the thing at pos, returning the result."
  (let ((loc (jsx-flow//pos-to-flow-location pos))
        (filename (buffer-file-name)))
    (apply #'jsx-flow//json-call-flow-on-current-buffer "find-refs" filename loc)))

(defun jsx-flow//type-at-pos-async (result-handler pos)
  "Calls flow to get the type at pos asynchronously; passes the result to result-handler."
  (let* ((loc (jsx-flow//pos-to-flow-location pos))
         (filename (buffer-file-name)))
    (apply #'jsx-flow//json-call-flow-on-current-buffer-async result-handler "type-at-pos" "--path" filename loc)))

(defun jsx-flow//type-at-pos (pos)
  "Calls flow to get the type at pos synchronously, returning the result."
  (let* ((loc (jsx-flow//pos-to-flow-location pos))
         (filename (buffer-file-name)))
    (apply #'jsx-flow//json-call-flow-on-current-buffer "type-at-pos" "--path" filename loc)))

(defun jsx-flow//ellipsize (s max-len)
  (if (<= (length s) max-len)
      s
    (concat
     (substring s 0 (- max-len 3))
     "...")))

(defun jsx-flow//flow-offset-to-pos (offset)
  (byte-to-position (1+ offset)))

(defun jsx-flow//eldoc-show-type-info (data)
  "Shows the passed type info using eldoc."
  (let* ((type (alist-get 'type data)))
    (when (not (equal "(unknown)" type))
      (let* ((start (jsx-flow//flow-offset-to-pos
                     (assq-recursive data 'loc 'start 'offset)))
             (end (jsx-flow//flow-offset-to-pos
                   (assq-recursive data 'loc 'end 'offset)))
             (text (buffer-substring start end))
             (text (replace-regexp-in-string "[[:space:]\n]+" " " text))
             (text (jsx-flow//ellipsize text 30)))
        (eldoc-message (concat text ": " type))))))

(defun jsx-flow/eldoc-show-type-at-point ()
  "Shows type at point."
  (interactive)
  (jsx-flow//type-at-pos-async #'jsx-flow//eldoc-show-type-info (point))
  nil)

(defvar jsx-flow--ast nil
  "The AST from flow.")

(defvar-local jsx-flow--ast-invalid-from nil)

(defvar-local jsx-flow-ast-hook nil
  "Hook called after an AST update.")

(defun jsx-flow//put-node-property (ast-node property value)
  (put-text-property (jsx-flow//node-start ast-node)
                     (jsx-flow//node-end ast-node)
                     property value)
  nil)

(defun jsx-flow//put-identifier-property (ast-node property value)
  (when (and ast-node (eq 'Identifier (jsx-flow//node-type ast-node)))
    (let ((beg (jsx-flow//node-start ast-node))
          (end (jsx-flow//node-end ast-node)))
      (when-let ((type-annot (jsx-flow//node-field ast-node 'typeAnnotation)))
        (setq end (jsx-flow//node-start type-annot)))
      (put-text-property beg end property value)))
  nil)

(defvar jsx-flow--propertize-min)
(defvar jsx-flow--propertize-max)

(defun jsx-flow//walk-ast-propertize (ast-node)
  (when (and (< (jsx-flow//node-start ast-node) jsx-flow--propertize-max)
             (< jsx-flow--propertize-min (jsx-flow//node-end ast-node)))
    (unless
        (pcase (jsx-flow//node-type ast-node)
          (`Literal
           (when (jsx-flow//node-field ast-node 'regex)
             (jsx-flow//put-node-property ast-node 'jsx-flow-prop 'regex)))

          ((or `VariableDeclarator `ClassDeclaration)
           (let ((id (jsx-flow//node-field ast-node 'id)))
             (jsx-flow//put-identifier-property id 'jsx-flow-prop 'var)))

          ((or `AssignmentPattern)
           (let ((left (jsx-flow//node-field ast-node 'left)))
             (jsx-flow//put-identifier-property left 'jsx-flow-prop 'var)))

          ((or `ImportSpecifier)
           (let ((left (jsx-flow//node-field ast-node 'local)))
             (jsx-flow//put-identifier-property left 'jsx-flow-prop 'var)))

          ((or `TypeAlias)
           (let ((id (jsx-flow//node-field ast-node 'id)))
             (jsx-flow//put-identifier-property id 'jsx-flow-prop 'type)))

          ((or `ClassProperty)
           (jsx-flow//put-identifier-property (jsx-flow//node-field ast-node 'key)
                                              'jsx-flow-prop 'var))

          ((or `FunctionExpression `ArrowFunctionExpression)
           (when-let ((id (jsx-flow//node-field ast-node 'id)))
             (jsx-flow//put-identifier-property id 'jsx-flow-prop 'var))
           (mapc (lambda (child) (jsx-flow//put-identifier-property child 'jsx-flow-prop 'var))
                 (jsx-flow//node-field ast-node 'params))
           nil)

          (`ObjectPattern
           (mapc (lambda (property)
                   (when-let ((val (jsx-flow//node-field property 'value)))
                     (when (eq 'Identifier (jsx-flow//node-type val))
                       (jsx-flow//put-identifier-property val 'jsx-flow-prop 'var))))
                 (jsx-flow//node-field ast-node 'properties))
           nil)

          ((or `GenericTypeAnnotation)
           (jsx-flow//put-node-property ast-node 'jsx-flow-prop 'type))

          (`TemplateLiteral
           (mapc (lambda (elem) (jsx-flow//put-node-property elem 'jsx-flow-prop 'template-text))
                 (jsx-flow//node-field ast-node 'quasis))
           nil)

          (`JSXText
           (jsx-flow//put-node-property ast-node 'jsx-flow-prop 'text)))
      (jsx-flow//visit-children #'jsx-flow//walk-ast-propertize ast-node))))

(defun jsx-flow//propertize-ast (beg limit)
  (with-silent-modifications
    (remove-list-of-text-properties beg limit '(jsx-flow-prop))
    (let ((jsx-flow--propertize-min beg)
          (jsx-flow--propertize-max limit))
      (jsx-flow//walk-ast-propertize jsx-flow--ast))))

(defun jsx-flow//extend-region-for-ast-update (beg end)
  (when-let ((jsx-elem (jsx-flow//deepest-jsx-element-containing beg)))
    (cons (min (jsx-flow//node-start jsx-elem) beg) end)))

(defun jsx-flow//jsx-element-p (ast-node)
  (eq (jsx-flow//node-type ast-node) 'JSXElement))

(defun jsx-flow//deepest-jsx-element-containing (pos)
  (car (last (cl-remove-if-not #'jsx-flow//jsx-element-p
                               (jsx-flow//node-path-at-pos pos)))))

(defun jsx-flow//receive-ast (data invalid-from)
  "Handler for the flow AST call."
  ;; ignore ast if it's already out of date
  ;; TODO: could use the part up to the new invalid-from
  (unless jsx-flow--ast-invalid-from
    (let ((ast (json-read-from-string data)))
      (setq jsx-flow--ast ast)
      (when-let ((extended (jsx-flow//extend-region-for-ast-update invalid-from (point-max))))
        (setq invalid-from (car extended)))
      (jsx-flow//propertize-ast invalid-from (point-max))
      (syntax-ppss-flush-cache invalid-from)
      (font-lock-flush invalid-from (point-max))
      (run-hooks jsx-flow-ast-hook))))

(defun jsx-flow//do-parse ()
  "Calls flow to get the AST and stores it in jsx-flow--ast."
  ;; (message "do-parse")
  (let ((invalid-from jsx-flow--ast-invalid-from))
    (setq jsx-flow--ast-invalid-from nil)
    (jsx-flow//call-flow-on-current-buffer-async
     (lambda (data)
       (jsx-flow//receive-ast data invalid-from)) "ast")))

;; flow coverage
(defvar-local jsx-flow-type-coverage t)
(defvar-local jsx-flow--coverage nil)

(defun jsx-flow//add-untyped-property (loc)
  (let ((beg (jsx-flow//flow-offset-to-pos (assq-recursive loc 'start 'offset)))
        (end (jsx-flow//flow-offset-to-pos (assq-recursive loc 'end 'offset))))
    (put-text-property beg end 'untyped t)))

(defun jsx-flow//coverage-propertize ()
  (with-silent-modifications
    (mapc #'jsx-flow//add-untyped-property (alist-get 'uncovered_locs jsx-flow--coverage))))

(defun jsx-flow//do-coverage ()
  "Calls flow to get the type coverage and stores it in jsx-flow--coverage."
  (jsx-flow//json-call-flow-async
   (lambda (data)
     (setq jsx-flow--coverage (alist-get 'expressions data))
     (jsx-flow//coverage-propertize)
     (font-lock-flush))
   "coverage" (buffer-file-name)))

(defun jsx-flow/toggle-type-coverage ()
  (interactive)
  (setq jsx-flow-type-coverage (not jsx-flow-type-coverage))
  (if jsx-flow-type-coverage
      (jsx-flow//do-coverage)
    (font-lock-flush)))

;; AST functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jsx-flow//node-start (node)
  "Returns the (inclusive) start position of node."
  (let ((pos (elt (cdr (assq 'range node)) 0)))
    (if (markerp pos)
        pos
      (if-let ((char-pos (jsx-flow//flow-offset-to-pos pos)))
          char-pos
        (point-max)))))

(defun jsx-flow//node-end (node)
  "Returns the (exclusive) end position of node."
  (let ((pos (elt (cdr (assq 'range node)) 1)))
    (if (markerp pos)
        pos
      (if-let ((char-pos (jsx-flow//flow-offset-to-pos pos)))
          char-pos
        (point-max)))))

(defun jsx-flow//node-field (node field)
  "Returns the given field of node."
  (cdr (assq field node)))

(defun jsx-flow//node-fields (node)
  "Returns all fields of node."
  (map 'list #'car node))

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
                    NullLiteralTypeAnnotation
                    BooleanTypeAnnotation
                    BooleanLiteralTypeAnnotation
                    AnyTypeAnnotation
                    MixedTypeAnnotation
                    ExistsTypeAnnotation ;; *

                    EmptyStatement
                    DebuggerStatement

                    Literal
                    ThisExpression
                    Super
                    RegExpLiteral
                    TemplateElement

                    JSXIdentifier
                    JSXEmptyExpression
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

(defun jsx-flow//fields-by-type (node-type)
  (pcase node-type
    ;; Expressions
    ((or `AssignmentExpression `BinaryExpression `LogicalExpression)
     '(left right))
    (`MemberExpression
     '(object property))
    ((or `FunctionExpression `ArrowFunctionExpression)
     '(id typeParameters params returnType body))
    (`ArrayExpression
     '(elements))
    (`ObjectExpression
     '(properties))
    (`Property
     '(key value))
    (`SequenceExpression
     '(expressions))
    ((or `UpdateExpression `UnaryExpression)
     '(argument))
    (`ConditionalExpression
     '(test alternate consequent))
    ((or `NewExpression `CallExpression)
     '(callee arguments))
    ((or `ClassDeclaration `ClassExpression)
     '(id superClass body implements typeParameters superTypeParameters))
    (`YieldExpression
     '(argument))

    ;; Template literals
    (`TemplateLiteral
     '(quasis expressions))
    (`TaggedTemplateExpression
     '(tag quasi))

    ;; Patterns
    (`ObjectPattern
     '(properties typeAnnotation))
    (`ArrayPattern
     '(elements typeAnnotation))
    (`RestElement
     '(argument typeAnnotation))
    (`AssignmentPattern
     '(left right))

    ;; Class declarations
    (`MethodDefinition
     '(key value decorators))
    (`ClassProperty
     '(key value typeAnnotation))
    (`MetaProperty
     '(meta property))

    ;; Statements
    (`ExpressionStatement
     '(expression))
    (`IfStatement
     '(test consequent alternate))
    (`LabeledStatement
     '(label body))
    ((or `ContinueStatement `BreakStatement)
     '(label))
    (`WithStatement
     '(object body))
    (`SwitchStatement
     '(discriminant cases))
    ((or `ThrowStatement `ReturnStatement)
     '(argument))
    (`TryStatement
     '(block handler finalizer))
    (`WhileStatement
     '(test body))
    (`DoWhileStatement
     '(body test))
    (`ForStatement
     '(init test update body))
    ((or `ForOfStatement `ForInStatement)
     '(left right body))
    (`FunctionDeclaration
     '(id typeParameters params returnType body))
    (`VariableDeclaration
     '(declarations))
    (`VariableDeclarator
     '(id init))

    ;; Clauses
    (`SwitchCase
     '(test consequent))
    (`CatchClause
     '(param body))

    ;; import/export
    (`ExportNamedDeclaration
     '(specifiers declaration source))
    (`ExportSpecifier
     '(local exported))
    (`ExportDefaultDeclaration
     '(declaration))
    (`ExportAllDeclaration
     '(source))
    (`ImportDeclaration
     '(specifiers source))
    (`ImportSpecifier
     '(local imported))
    ((or `ImportDefaultSpecifier `ImportNamespaceSpecifier)
     '(local))

    ;; Type annotations
    ((or `TypeAnnotation `Identifier `NullableTypeAnnotation)
     '(typeAnnotation))
    (`TypeAlias
     '(id typeParameters right))
    ((or `IntersectionTypeAnnotation `UnionTypeAnnotation `TupleTypeAnnotation)
     '(types))
    (`TypeofTypeAnnotation
     '(argument))
    (`GenericTypeAnnotation
     '(id typeParameters))
    (`ArrayTypeAnnotation
     '(elementType))
    (`ObjectTypeAnnotation
     '(properties indexers callProperties))
    (`ObjectTypeProperty
     '(key value))
    (`ObjectTypeIndexer
     '(id key value))
    (`ObjectTypeCallProperty
     '(value))
    (`FunctionTypeAnnotation
     '(typeParameters params rest returnType))
    (`FunctionTypeParam
     '(name typeAnnotation))
    (`TypeParameter
     '(bound default))
    (`TypeCastExpression
     '(expression typeAnnotation))
    ((or `TypeParameterInstantiation `TypeParameterDeclaration)
     '(params))
    (`QualifiedTypeIdentifier
     '(qualification id))

    ;; flow type declarations
    (`DeclareClass
     '(id typeParameters body extends))
    (`DeclareModule
     '(id body))
    (`DeclareVariable
     '(id))
    (`DeclareFunction
     '(id predicate))
    (`InterfaceExtends
     '(id typeParameters))

    ;; JSX
    (`JSXElement
     '(openingElement children closingElement))
    (`JSXOpeningElement
     '(name attributes))
    (`JSXClosingElement
     '(name))
    (`JSXAttribute
     '(name value))
    (`JSXExpressionContainer
     '(expression))
    (`JSXSpreadAttribute
     '(argument))
    (`JSXNamespacedName
     '(namespace name))))

(defun jsx-flow//visit-children (fun ast-node)
  "Runs fun on each of ast-node's children in turn."
  (pcase (jsx-flow//node-type ast-node)
    ((pred jsx-flow//leaf-p)
     nil)
    ((pred jsx-flow//body-p)
     (jsx-flow//visit-fields '(body) fun ast-node))

    (other
     (if-let ((fields (jsx-flow//fields-by-type other)))
         (jsx-flow//visit-fields fields fun ast-node)
       (message "Unknown node type: %s %s" other (jsx-flow//node-fields ast-node))))))

(defun jsx-flow//jsx-matching-element (jsx-element)
  "Returns the closing element for an opening element and vice versa."
  (let ((parent (jsx-flow//node-parent jsx-element)))
    (case (jsx-flow//node-type jsx-element)
      ('JSXOpeningElement (jsx-flow//jsx-closing-element parent))
      ('JSXClosingElement (jsx-flow//jsx-opening-element parent)))))

(defun jsx-flow//node-path-at-pos (pos &optional ast-node)
  "Returns the path of AST nodes at pos."
  (let* ((ast-node (or ast-node jsx-flow--ast))
         (path nil))
    (when (<= (jsx-flow//node-start ast-node) pos (1- (jsx-flow//node-end ast-node)))
      (jsx-flow//visit-children (lambda (node)
                            (let ((found-path (jsx-flow//node-path-at-pos pos node)))
                              (when found-path
                                (setq path found-path))))
                          ast-node)
      (cons ast-node path))))

(defun jsx-flow//node-path-to-node (node &optional ast-node)
  "Returns the path of AST nodes at pos."
  (let* ((ast-node (or ast-node jsx-flow--ast))
         (path nil))
    (if (eq node ast-node)
        (list node)
      (when (<= (jsx-flow//node-start ast-node) (jsx-flow//node-start node)
                (jsx-flow//node-end node) (jsx-flow//node-end ast-node))
        (jsx-flow//visit-children
         (lambda (child)
           (let ((found-path (jsx-flow//node-path-to-node node child)))
             (when found-path
               (setq path found-path))))
         ast-node)
        (cons ast-node path)))))

(defun jsx-flow//node-parent (node &optional ast-node)
  "Returns the parent of node."
  (let* ((ast-node (or ast-node jsx-flow--ast))
         (parent nil))
    (when (<= (jsx-flow//node-start ast-node) (jsx-flow//node-start node)
              (jsx-flow//node-end node) (jsx-flow//node-end ast-node))
      (jsx-flow//visit-children
       (lambda (child)
         (if (eq child node)
             (setq parent ast-node)
           (let ((found-parent (jsx-flow//node-parent node child)))
             (when found-parent
               (setq parent found-parent)))))
       ast-node)
      parent)))

(defun jsx-flow//node-path-string (nodes)
  (mapconcat (lambda (node) (symbol-name (jsx-flow//node-type node))) nodes " -> "))

(defun jsx-flow/print-node-path-at-point ()
  (interactive)
  (message "%s" (jsx-flow//node-path-string (jsx-flow//node-path-at-pos (point))))
  nil)

(defun jsx-flow//print-nodes (nodes)
  (dolist (node nodes)
    (message "Node: %s" (jsx-flow//node-type node))))

(defun jsx-flow//find-deepest-node (pos)
  "Returns the deepest AST node containing pos."
  (car (last (jsx-flow//node-path-at-pos pos))))

;; company provider

(defun jsx-flow//fetch-completion-json ()
  (let* ((loc (jsx-flow//pos-to-flow-location (point)))
         (filename (buffer-file-name)))
    (jsx-flow//json-call-flow-on-current-buffer "autocomplete" filename (car loc) (cadr loc))))

(defun jsx-flow//make-completion-candidate (candidate)
  (let ((name (cdr (assoc 'name candidate)))
        (type (cdr (assoc 'type candidate)))
        (path (cdr (assoc 'path candidate)))
        (line (cdr (assoc 'line candidate))))
    (propertize name
                'type type
                'path path
                'line line)))

(defun jsx-flow//fetch-completions (&rest _)
  (let* ((response (jsx-flow//fetch-completion-json))
         (result (cdr (assoc 'result response)))
         (names (mapcar #'jsx-flow//make-completion-candidate result)))
    names))

(defun jsx-flow//get-completion-annotation (candidate)
  (format " (%s)" (get-text-property 0 'type candidate)))

(defun company-jsx-flow-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (case command
    (interactive (company-begin-backend 'company-jsx-flow-backend))
    (prefix (and (eq major-mode 'jsx-flow-mode)
                 (company-grab-symbol-cons "\\." 2)))
    (candidates
     (progn
       ;; (message "candidates %s" arg)
       (let* ((completes (jsx-flow//fetch-completions))
              ;; (_ (message "names %s" completes))
              (list (cl-remove-if-not
                    (lambda (c) (string-prefix-p arg c))
                    completes)))
         ;; (message "list %s" list)
         list)))
    (annotation (jsx-flow//get-completion-annotation arg))
    (location (cons (get-text-property 0 'path arg)
                    (get-text-property 0 'line arg)))))

;; flycheck

(with-eval-after-load 'flycheck
  (defun jsx-flow//fc-convert-error (error checker counter buffer)
    "Return a list of errors from ERROR."
    (let* ((msg-parts (cdr (assoc 'message error)))
           (first-part (elt msg-parts 0))
           (level (if (eq (cdr (assoc 'level first-part)) "warning") 'warning 'error))
           (file (cdr (assoc 'path first-part)))
           (line (cdr (assoc 'line first-part)))
           (col  (cdr (assoc 'start first-part)))
           (desc (--reduce (format "%s\n%s" acc it) (--map (cdr (assoc 'descr it)) msg-parts))))
      (when (string= file (buffer-file-name buffer))
        (list (flycheck-error-new-at line col level desc :checker checker :id counter)))))

  (defun jsx-flow//parse-status-errors (json checker buffer)
    "Parse flow status errors in OUTPUT."
    (let* ((errors (cdr (assoc 'errors json)))
           (counter 0))
      (-mapcat
       (lambda (err)
         (setq counter (1+ counter))
         (jsx-flow//fc-convert-error err checker (number-to-string counter) buffer))
       errors)))

  (defun jsx-flow//check-flow (checker report)
    (let ((buffer (current-buffer)))
      (jsx-flow//json-call-flow-async
       (lambda (status)
         (let ((errors (jsx-flow//parse-status-errors status checker buffer)))
           (funcall report 'finished errors)))
       "status")))

  (flycheck-define-generic-checker 'javascript-flow
    "A JavaScript syntax and style checker using Flow."
    :start #'jsx-flow//check-flow
    ;; TODO :interrupt handler
    ;; TODO :verify
    ;; TODO :predicate checking for @flow comment?
    ;; could just do AST check otherwise
    :next-checkers '((error . javascript-eslint))
    :modes '(jsx-flow-mode))

  (flycheck-add-mode 'javascript-eslint 'jsx-flow-mode)
  (add-to-list 'flycheck-checkers 'javascript-flow))


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

(defconst jsx-flow--declare-keyword-re
  "\\(declare\\)[[:space:]]+\\(module\\|class\\|var\\|const\\|function\\|type\\)"
  "Regexp matching the declare keyword for flow declarations.")

(defconst jsx-flow--basic-type-re
  (js--regexp-opt-symbol
   '("boolean" "number" "string" "any" "void" "mixed"))
  "Regular expression matching any predefined type in Flow.")

(defconst jsx-flow--constant-re
  (js--regexp-opt-symbol '("false" "null" "undefined"
                           "Infinity" "NaN"
                           "true" "arguments" "this"))
  "Regular expression matching any future reserved words in JavaScript.")

(defconst jsx-flow--opening-element-re
  "\\(<\\)\\([[:alnum:]]+\\)\\([[:space:]\n]+[[:alnum:]/]\\|>\\)")

(defconst jsx-flow--closing-element-re
  "\\(</\\)\\([[:alnum:]]+\\)\\(>\\)")

(defconst jsx-flow--jsx-attribute-re
  "\\([[:alnum:]-]+\\)\\|[{/>]")

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

(defun jsx-flow//jump-over-jsx-attribute-value (_limit)
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

(defun jsx-flow//font-lock-extend-region ()
  ;; TODO: improve this!
  (unless (= font-lock-beg (point-min))
    (setq font-lock-beg (point-min))
    t))

(defun jsx-flow//match-to-limit (limit)
  (unless (= limit (point))
    (let ((beg (point)))
      (goto-char limit)
      (set-match-data (list beg limit))
      t)))

(defun jsx-flow//find-ast-prop (limit)
  (let ((beg (text-property-not-all (point) limit 'jsx-flow-prop nil)))
    (when beg
      (let ((end (next-single-property-change beg 'jsx-flow-prop nil limit)))
        (goto-char end)
        (set-match-data (list beg end))
        t))))

(defun jsx-flow//determine-ast-face ()
  (case (get-text-property (match-beginning 0) 'jsx-flow-prop)
    ('var 'font-lock-variable-name-face)
    ('type 'font-lock-type-face)))

(defun jsx-flow//find-coverage-prop (limit)
  (when jsx-flow-type-coverage
    (let ((beg (text-property-not-all (point) limit 'untyped nil)))
      (when beg
        (let ((end (next-single-property-change beg 'untyped nil limit)))
          (goto-char end)
          (set-match-data (list beg end))
          t)))))

(defconst jsx-flow--font-lock-keywords-2
  `((,jsx-flow--keyword-re . 'font-lock-keyword-face)
    (,jsx-flow--declare-keyword-re (1 'font-lock-keyword-face))
    (,jsx-flow--basic-type-re . 'font-lock-type-face)
    (,jsx-flow--constant-re . 'font-lock-constant-face)

    ;; opening / self-closing jsx elements
    (,jsx-flow--opening-element-re
     (1 'jsx-flow-jsx-tag-bracket-face)
     (2 'jsx-flow-jsx-tag-face)
     ;; color attributes
     (jsx-flow//find-next-jsx-attribute
      ;; jump to beginning of attribute list,
      ;; and allow searching until end of file
      (progn (goto-char (match-end 2)) (point-max))
      ;; go back to beginning of attribute list again, so we can color jsx tags
      ;; within attributes
      (goto-char (match-end 2))
      (0 'jsx-flow-jsx-attribute-face))
     ;; color closing bracket
     (jsx-flow//match-to-limit
      ;; jump to beginning of attribute list, then search for end of tag. Save
      ;; match data so we can use it again in the post function.
      (progn (save-match-data
               (goto-char (match-end 2))
               (jsx-flow//jump-to-end-of-jsx-tag (point-max))
               (goto-char (match-beginning 0))
               (match-end 0)))
      ;; go back to beginning of attribute list again, so we can color jsx tags
      ;; within attributes
      (goto-char (match-end 2))
      (0 'jsx-flow-jsx-tag-bracket-face)))
    ;; closing jsx elements
    (,jsx-flow--closing-element-re
     (1 'jsx-flow-jsx-tag-bracket-face)
     (2 'jsx-flow-jsx-tag-face)
     (3 'jsx-flow-jsx-tag-bracket-face))
    ,@js--font-lock-keywords-1)
  "Level two font lock keywords for `js-mode'.")

(defconst jsx-flow--font-lock-keywords-ast
  `((jsx-flow//find-ast-prop . (0 (jsx-flow//determine-ast-face)))
    ,@jsx-flow--font-lock-keywords-2
    (jsx-flow//find-coverage-prop . (0 'jsx-flow-untyped-face prepend))))

(defconst jsx-flow-mode-syntax-table js-mode-syntax-table)

(defun jsx-flow//syntax-propertize (start end)
  (save-excursion
    (let ((pos start))
      (while (< pos end)
        (setq pos (next-single-property-change pos 'jsx-flow-prop nil end))
        (let ((prev (get-text-property (1- pos) 'jsx-flow-prop))
              (next (get-text-property pos 'jsx-flow-prop)))
          (unless (eq prev next)
            (case prev
              ((text template-text regex)
               (put-text-property (1- pos) pos 'syntax-table '(15))))
            (case next
              ((text template-text regex)
               (put-text-property pos (1+ pos) 'syntax-table '(15))))))))))

;;; Indentation (mostly copied from js-mode)
(defun jsx-flow//indent-line ()
  "Indent the current line as JSX (with SGML offsets).
i.e., customize JSX element indentation with `sgml-basic-offset',
`sgml-attribute-offset' et al."
  (interactive)
  (let ((indentation-type (js--jsx-indented-element-p)))
    (cond
     ((eq indentation-type 'expression)
      (js--expression-in-sgml-indent-line))
     ((or (eq indentation-type 'first)
          (eq indentation-type 'after))
      ;; Don't treat this first thing as a continued expression (often a "<" or
      ;; ">" causes this misinterpretation)
      (cl-letf (((symbol-function #'js--continued-expression-p) 'ignore))
        (js-indent-line)))
     ((eq indentation-type 'nth)
      (js--as-sgml (sgml-indent-line)))
     (t (jsx-flow//indent-js-line)))))

(defun jsx-flow//indent-js-line ()
  ;; TODO ensure up-to-date parse tree
  ;; (parse tree is currently unused though)
  (let ((path (jsx-flow//node-path-at-pos (point-at-bol)))
        (parse-status (save-excursion (syntax-ppss (point-at-bol))))
        (offset (- (point) (save-excursion (back-to-indentation) (point)))))
    (unless (nth 3 parse-status)
      (indent-line-to (jsx-flow//proper-indentation parse-status path))
      (when (> offset 0) (forward-char offset)))))

(defun jsx-flow//proper-indentation (parse-status _node-path)
  (save-excursion
    (back-to-indentation)
    (cond
     ((nth 4 parse-status)    ; inside comment
      (js--get-c-offset 'c (nth 8 parse-status)))
     ((nth 3 parse-status) 0) ; inside string

     ((js--ctrl-statement-indentation))
     ((js--multi-line-declaration-indentation))

     ((nth 1 parse-status)
      ;; A single closing paren/bracket should be indented at the
      ;; same level as the opening statement. Same goes for
      ;; "case" and "default".
      (let ((same-indent-p (looking-at "[]})]"))
            (switch-keyword-p (looking-at "default\\_>\\|case\\_>[^:]"))
            (continued-expr-p (js--continued-expression-p)))
        (goto-char (nth 1 parse-status)) ; go to the opening char
        (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
            (progn ; nothing following the opening paren/bracket
              (skip-syntax-backward " ")
              (when (eq (char-before) ?\)) (backward-list))
              (back-to-indentation)
              (js--maybe-goto-declaration-keyword-end parse-status)
              (let* ((in-switch-p (unless same-indent-p
                                    (looking-at "\\_<switch\\_>")))
                     (same-indent-p (or same-indent-p
                                        (and switch-keyword-p
                                             in-switch-p)))
                     (indent
                      (cond (same-indent-p
                             (current-column))
                            (continued-expr-p
                             (+ (current-column) (* 2 js-indent-level)
                                js-expr-indent-offset))
                            (t
                             (+ (current-column) js-indent-level
                                (pcase (char-after (nth 1 parse-status))
                                  (?\( js-paren-indent-offset)
                                  (?\[ js-square-indent-offset)
                                  (?\{ js-curly-indent-offset)))))))
                (if in-switch-p
                    (+ indent js-switch-indent-offset)
                  indent)))
          ;; If there is something following the opening
          ;; paren/bracket, everything else should be indented at
          ;; the same level.
          (unless same-indent-p
            (forward-char)
            (skip-chars-forward " \t"))
          (current-column))))

     ((js--continued-expression-p)
      (+ js-indent-level js-expr-indent-offset))

     (t 0)
     )))



(defvar jsx-flow--parse-timer nil)

(defun jsx-flow//after-change (beg _end _replaced-len)
  ;; invalidate AST from beg to end of file
  (when (or (null jsx-flow--ast-invalid-from) (< beg jsx-flow--ast-invalid-from))
    (setq jsx-flow--ast-invalid-from beg)))

;;;###autoload
(define-derived-mode jsx-flow-mode
  prog-mode "JSX/Flow"
  "Major mode for JavaScript with Flow and JSX."

  ;; TODO this can go away soon
  (with-silent-modifications
    (set-text-properties (point-min) (point-max) nil))

  (setq-local open-paren-in-column-0-is-defun-start nil)
  (setq-local syntax-propertize-function 'jsx-flow//syntax-propertize)
  (setq-local font-lock-defaults '((jsx-flow--font-lock-keywords-ast)))
  (setq-local font-lock-extend-region-functions '(jsx-flow//font-lock-extend-region))
  (setq-local indent-line-function 'jsx-flow//indent-line)

  (setq-local parse-sexp-ignore-comments t)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local comment-start "// ")
  (setq-local comment-end "")

  (setq-local fill-paragraph-function 'js-c-fill-paragraph)

  ;; electric indent after > for closing JSX tags
  (setq-local electric-indent-chars '(10 ?>))

  ;; do filling with cc-mode for now (copied from js-mode)
  (setq c-comment-prefix-regexp "//+\\|\\**"
        c-paragraph-start "\\(@[[:alpha:]]+\\>\\|$\\)"
        c-paragraph-separate "$"
        c-block-comment-prefix "* "
        c-line-comment-starter "//"
        c-comment-start-regexp "/[*/]\\|\\s!"
        comment-start-skip "\\(//+\\|/\\*+\\)\\s *")

  (let ((c-buffer-is-cc-mode t))
    ;; FIXME: These are normally set by `c-basic-common-init'.  Should
    ;; we call it instead?  (Bug#6071)
    (make-local-variable 'paragraph-start)
    (make-local-variable 'paragraph-separate)
    (make-local-variable 'paragraph-ignore-fill-prefix)
    (make-local-variable 'adaptive-fill-mode)
    (make-local-variable 'adaptive-fill-regexp)
    (c-setup-paragraph-variables))

  ;; eldoc
  (set (make-local-variable 'eldoc-documentation-function) #'jsx-flow/eldoc-show-type-at-point)
  (eldoc-mode 1)

  ;; parsing
  (setq jsx-flow--ast nil)
  (setq jsx-flow--ast-invalid-from (point-min))
  (jsx-flow//do-parse)
  (add-hook 'after-change-functions #'jsx-flow//after-change t t)

  (when jsx-flow-type-coverage
    (jsx-flow//do-coverage))
  (add-hook 'after-save-hook #'jsx-flow//do-coverage nil t)

  (when (null jsx-flow--parse-timer)
    (setq jsx-flow--parse-timer
          (run-with-idle-timer 0.2 t
                               (lambda ()
                                 (when jsx-flow--ast-invalid-from
                                   (jsx-flow//do-parse))))))

  ;; company
  (add-to-list (make-local-variable 'company-backends) 'company-jsx-flow-backend)

  (flycheck-mode 1))

(provide 'jsx-flow-mode)
;;; jsx-flow-mode.el ends here
