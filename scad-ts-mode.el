;;; scad-ts-mode.el --- Tree-sitter support for OpenSCAD -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Yoshinari Nomura

;; Author: Yoshinari Nomura <nom@quickhack.net>
;; Maintainer: Yoshinari Nomura <nom@quickhack.net>
;; Created: December 2025
;; Version: 0.1.0
;; Keywords: OpenSCAD languages tree-sitter
;; URL: https://github.com/yoshinari-nomura/scad-ts-mode
;; Package-Requires: ((emacs "29.3"))

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  This program requires ~/.emacs.d/tree-sitter/libtree-sitter-openscad.so
;;  See: https://github.com/openscad/tree-sitter-openscad/

;;; Code:

(require 'treesit)

(defcustom scad-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `scat-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'scad)

;; * Tree-sitter node manipulation
;;
;; Retrieving Nodes (GNU Emacs Lisp Reference Manual)
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Retrieving-Nodes.html
;; Accessing Node Information (GNU Emacs Lisp Reference Manual)
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Accessing-Node-Information.html

(defun scad-ts-mode--node-toplevel-p (node)
  "Return t if Tree-sitter NODE is on the Top-level."
  (string-equal
   "source_file"
   (treesit-node-type (treesit-node-parent node))))

(defun scad-ts-mode--node-name-text (node)
  "Return text in name: from Tree-sitter NODE.
Code: function function_name()...
NODE: (function_item function name: (identifier)...
Return: \"function_name\""
  (treesit-node-text
   (treesit-node-child-by-field-name node "name")
   t))

(defun scad-ts-mode--node-child-name-text (node)
  "Perform `scad-ts-mode-node-name-text' in NODE's child.
Code: VARNAME = 3;
NODE: (var_declaration
        (assignment name: (identifier) = value: (integer))...
Return: \"VARNAME\""
  (scad-ts-mode--node-name-text (treesit-node-child node 0)))

(defun scad-ts-mode--any (symbols)
  "Return regexp to strict-match a string in the list SYMBOLS.
See `regexp-opt' for details."
  (concat "\\`" (regexp-opt (mapcar #'symbol-name symbols)) "\\'"))

;; * Syntax table
;;
;; Syntax Class Table (GNU Emacs Lisp Reference Manual)
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html

(defvar scad-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; Taken from the c-ts-mode
    (modify-syntax-entry ?_    "_"      table)
    (modify-syntax-entry ?\\   "\\"     table)
    (modify-syntax-entry ?+    "."      table)
    (modify-syntax-entry ?-    "."      table)
    (modify-syntax-entry ?=    "."      table)
    (modify-syntax-entry ?%    "."      table)
    (modify-syntax-entry ?<    "."      table)
    (modify-syntax-entry ?>    "."      table)
    (modify-syntax-entry ?&    "."      table)
    (modify-syntax-entry ?|    "."      table)
    (modify-syntax-entry ?\240 "."      table) ;; nbsp
    (modify-syntax-entry ?/    ". 124b" table)
    (modify-syntax-entry ?*    ". 23"   table)
    (modify-syntax-entry ?\n   "> b"    table)
    table)
  "Syntax table for `scad-ts-mode'.")

;; * Indent rules
;;
;; + Parser-based Indentation (GNU Emacs Lisp Reference Manual)
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser_002dbased-Indentation.html

(defvar scad-ts-mode--indent-rules
  `((openscad
     ;; No indent at the Top-level
     ((parent-is "source_file") column-0 0)

     ;; Parentheses on separate lines should be aligned with their
     ;; parents:
     ;;
     ;;   module test(param1=1, param2=2
     ;;               param3=3) {
     ;;               ^-- (parent-bol)
     ;;     ...
     ;;   } <-- HERE (standalone-parent)
     ;;
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") standalone-parent 0)
     ((node-is "else") standalone-parent 0)

     ;; Indent the inside of union/if block or transform_chain
     ((parent-is "block") standalone-parent scad-ts-mode-indent-offset)
     ((parent-is "transform_chain") parent-bol scad-ts-mode-indent-offset)

     ;; Second or later arguments/list-values follow the first sibling
     ((match nil "arguments" nil 2 nil) (nth-sibling 1) 0)
     ((match nil "parameters" nil 2 nil) (nth-sibling 1) 0)
     ((match nil "list" nil 2 nil) (nth-sibling 1) 0)

     ;; First argument/list-value should be indented
     ((parent-is "arguments") parent-bol scad-ts-mode-indent-offset)
     ((parent-is "parameters")  (nth-sibling 0) 1)
     ((parent-is "list") parent-bol scad-ts-mode-indent-offset)

     ;; If the node is inside an Error, the most likely situation is
     ;; that you pressed TAB while an if-statement or array-block is
     ;; still open (in the middle of input):
     ;;
     ;; if (x == 3) {
     ;; TAB <- HERE
     ;;
     ;; In such a case, it would make sense to indent because the user
     ;; pressed the TAB anyway.
     ;;
     ((parent-is "ERROR") parent-bol scad-ts-mode-indent-offset)
     ((node-is "ERROR") parent-bol scad-ts-mode-indent-offset)

     (no-node parent-bol scad-ts-mode-indent-offset)))
  "Tree-sitter indent rules for `scad-ts-mode'.")

;; * Font-locking
;;
;; + Parser-based Font Lock (GNU Emacs Lisp Reference Manual)
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser_002dbased-Font-Lock.html
;;
;; + Pattern Matching (GNU Emacs Lisp Reference Manual)
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Pattern-Matching.html
;;
;; + Predicates and Directives - Tree-sitter
;;   https://tree-sitter.github.io/tree-sitter/using-parsers/queries/3-predicates-and-directives.html
;;
;; + tree-sitter-openscad/queries/highlights.scm at main
;;   https://github.com/openscad/tree-sitter-openscad/blob/main/queries/highlights.scm

(defvar scad-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   ;; 1) Comment:
   ;;    modifier "*" acts like a comment block
   :language 'openscad
   :feature 'comment
   '([(line_comment)
      (block_comment)
      (transform_chain (modifier "*"))]
     @font-lock-comment-face)

   :language 'openscad
   :feature 'comment
   '((modifier ["*" "!" "#" "%"] @font-lock-comment-face))

   ;; 1) Definition: function/module/variable/parameter definition
   :language 'openscad
   :feature 'definition
   '([(function_item name: (_) @font-lock-function-name-face)
      (module_item name: (_) @font-lock-function-name-face)
      (var_declaration (assignment name: (_) @font-lock-variable-name-face))
      ;; module name(*a* = 1, *b* = 2)
      (parameters
       (parameter
        (assignment name: (_) @font-lock-variable-name-face)))
      ;; module name(*a*, *b*)
      (parameters
       (parameter
        (identifier) @font-lock-variable-name-face))])

   ;; 2) Builtin: builtin special-variables
   :language 'openscad
   :feature 'builtin
   :override t
   `(((special_variable "$" (_)) @font-lock-builtin-face
      (:match ,(scad-ts-mode--any '(
        $fs $fn $t $vpr $vpt $vpd $vpf $children $preview))
        @font-lock-builtin-face)))

   ;; 2) Builtin: builtin functions/operations
   :language 'openscad
   :feature 'builtin
   `((module_call name: (_) @font-lock-builtin-face
       (:match ,(scad-ts-mode--any '(
        circle color cube cylinder difference
        hull intersection linear_extrude minkowski
        mirror multmatrix offset polygon polyhedron
        projection resize rotate rotate_extrude scale
        sphere square surface text translate union
        echo render children import))
               @font-lock-builtin-face)))

   ;; 2) Keyword
   :language 'openscad
   :feature 'keyword
   '((["module" "function" "let" "assign" "each"] @font-lock-keyword-face)
     ([(assert_statement "assert") (assert_expression "assert")]
      @font-lock-keyword-face)
     ((boolean) @font-lock-keyword-face)
     (["if" "else"] @font-lock-keyword-face)
     (["for" "intersection_for"] @font-lock-keyword-face))

   ;; 2) Preprocessor: include and use
   :language 'openscad
   :feature 'preprocessor
   '([(include_statement) (use_statement)] @font-lock-preprocessor-face)

   ;; 2) String
   :language 'openscad
   :feature 'string
   '((string) @font-lock-string-face)

   ;; 3) Constant: PI/undef
   :language 'openscad
   :feature 'constant
   '(((identifier) @font-lock-constant-face
      (:equal @font-lock-constant-face "PI"))
     (undef) @font-lock-constant-face
     ;; amodule(*name*=value): name is a kind of symbol
     (arguments (assignment name: (_) @font-lock-constant-face)))

   ;; 3) Escape-sequence: \n \t ... in String
   :language 'openscad
   :override t
   :feature 'escape-sequence
   '((escape_sequence) @font-lock-escape-face)

   ;; 4) Literal: number
   :language 'openscad
   :feature 'literal
   '([(integer) (float)] @font-lock-number-face)

   ;; 4) Bracket
   :language 'openscad
   :feature 'bracket
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

   ;; 4) Delimiter
   :language 'openscad
   :feature 'delimiter
   '([";" "," "."] @font-lock-delimiter-face)

   ;; 4) Function call
   :language 'openscad
   :feature 'function
   '([(module_call name: (identifier) @font-lock-function-call-face)
      (function_call name: (identifier) @font-lock-function-call-face)])

   ;; 4) Operator
   :language 'openscad
   :feature 'operator
   '((["||" "&&" "==" "!=" "<" ">" "<=" ">=" "+" "-" "*" "/" "%" "^" "!" ":" "="]
      @font-lock-operator-face)
     ((ternary_expression ["?" ":"] @font-lock-operator-face))))
  "Tree-sitter font-lock settings for `scad-ts-mode'.")

;;;###autoload
(defun scad-ts-mode-install-language-grammar ()
  "Install compatible libtree-sitter-openscad.so.
This command requires git and C compiler."
  (interactive)
  (let* ((workdir (make-temp-file "scad-ts-workdir" t))
         ;; XXX: Last commit of ABI 14 compatible.
         (treesit-language-source-alist `((openscad ,workdir "270e5ff"))))
    (unwind-protect
        (with-temp-buffer
          (treesit--call-process-signal
           "git" nil t nil "clone"
           "https://github.com/openscad/tree-sitter-openscad.git" workdir)
          (treesit-install-language-grammar 'openscad))
      (delete-directory workdir t))))

;;;###autoload
(define-derived-mode scad-ts-mode prog-mode "SCAD[ts]"
  "Major mode for OpenSCAD files using tree-sitter.

To use tree-sitter OpenSCAD modes by default, evaluate

  (add-to-list \\='major-mode-remap-alist \\='(scad-mode . scad-ts-mode))

in your init files.

Since this mode uses a parser, unbalanced brackets might cause some
breakage in indentation/fontification.  Therefore, it's recommended to
enable `electric-pair-mode' with this mode.

It is also recommended to use it together with openscad-lsp and Emacs'
standard `eglot'.  Eglot will work seamlessly with `flymake-mode',
`eldoc', and `imenu'."
  :group 'scad
  :syntax-table scad-ts-mode--syntax-table

  (when (treesit-ready-p 'openscad)
    (treesit-parser-create 'openscad)

    ;; Navigation like `treesit-beginning-of-defun'.
    (setq-local treesit-defun-type-regexp
                (regexp-opt '("function_item"
                              "module_item")))

    ;; Indent
    (setq-local treesit-simple-indent-rules scad-ts-mode--indent-rules)

    ;; Electric
    (setq-local electric-indent-chars
                (append "{}();" electric-indent-chars))

    ;; Imenu
    (setq-local treesit-simple-imenu-settings
                `(("Function" "\\`function_item\\'"
                   nil scad-ts-mode--node-name-text)
                  ("Module" "\\`module_item\\'"
                   nil scad-ts-mode--node-name-text)
                  ("Variable" "\\`var_declaration\\'"
                   scad-ts-mode--node-toplevel-p
                   scad-ts-mode--node-child-name-text)))

    ;; Font-lock
    (setq-local treesit-font-lock-settings scad-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment definition)
                  (builtin keyword preprocessor string)
                  (constant escape-sequence literal)
                  (bracket delimiter function operator)))

    (treesit-major-mode-setup)))

(provide 'scad-ts-mode)

;;; scad-ts-mode.el ends here
