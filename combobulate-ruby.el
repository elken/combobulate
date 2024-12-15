;;; combobulate-ruby.el --- ruby-specific features for combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-23  Mickey Petersen

;; Author: Mickey Petersen <mickey at masteringemacs.org>
;; Package-Requires: ((emacs "29"))
;; Version: 0.1
;; Homepage: https://www.github.com/mickeynp/combobulate
;; Keywords: convenience, tools, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun combobulate-ruby--get-definition (node)
  (string-join
   (combobulate-query-node-text
    (pcase (combobulate-node-type node)
      ("method"
       '((_) name: (_) @name parameters: (_) @args))
      ("class"
       '((_) name: (_) @name superclass: (constant) @args))
      ("module"
       '((_) name: (_) @name)))
    node
    t)
   ""))

(defun combobulate-ruby-pretty-print-node-name (node default-name)
  "Pretty printer for Ruby nodes"
  (combobulate-string-truncate
   (replace-regexp-in-string
    (rx (| (>= 2 " ") "\n")) ""
    (pcase (combobulate-node-type node)
      ("method" (concat "def " (combobulate-ruby--get-definition node)))
      ("class" (concat "class " (combobulate-ruby--get-definition node)))
      ("module" (concat "module " (combobulate-ruby--get-definition node)))
      (_ default-name)))
   40))

(defun combobulate-ruby-envelope-deindent-level ()
  "Determine the next-closest indentation level to deindent to."
  (car-safe (last (seq-take-while (lambda (num) (< num (current-column)))
                                  (nconc (number-sequence (prog-first-column) (1- (ruby-calculate-indent))
							  ruby-indent-level)
					 (list (ruby-calculate-indent)))))))

(defun combobulate-ruby-calculate-indent (pos)
  (let ((calculated-indentation (save-excursion
                                  (goto-char pos)
                                  (combobulate-filter-nodes
                                   (combobulate-get-parents
                                    (combobulate-node-at-point))))))
    (if (null calculated-indentation)
        (current-indentation)
      (* (ruby-calculate-indent) (length calculated-indentation)))))

(eval-and-compile
  (defconst combobulate-ruby-definitions
    '((context-nodes '("identifier"))
      (highlight-queries-default nil)
      (pretty-print-node-name-function #'combobulate-ruby-pretty-print-node-name)
      (envelope-procedure-shorthand-alist
       '((calls . ((:activation-nodes
		    ((:nodes ((rule "identifier") "call"))))))
	 (iterables . ((:activation-nodes
			((:nodes ((rule "array") (rule "range") (rule "hash") (rule "identifier") "call"))))))))
      (envelope-list
       `((:description
	  "if ... [else ...] end"
	  :key "bi"
	  :mark-node t
	  :shorthand calls
	  :name "nest-if-else"
	  :template
	  ((save-column
	    "if " @ n>
	    (choice* :name "if-block" :missing (@ "nil") :rest (r>))
	    n)
	   "else" n>
	   (choice* :name "else-block" :missing (@ "nil" n>) :rest (r> n>))
	   "end"))
	 (:description
	  "begin ... rescue ... end"
	  :key "bbr"
	  :mark-node t
	  :shorthand calls
	  :name "nest-begin-rescue"
	  :template
	  ((save-column
	    @ "begin" n>
	    (choice* :name "expression-block"
		     :missing
		     (@ "nil")
		     :rest
		     (r>))
	    n)
	   "rescue " (p StandardError "Exception" (lambda (in)
						    (concat in " => e")))
	   n>
	   (choice* :name "handler-block"
		    :missing
		    (@ "nil" n>)
		    :rest
		    (r> n>))
	   "end"))
	 (:description
	  "begin ... ensure ... end"
	  :key "bbe"
	  :mark-node t
	  :shorthand calls
	  :name "nest-begin-ensure"
	  :template
	  ((save-column
	    @ "begin" n>
	    (choice* :name "expression-block"
		     :missing
		     (@ "nil")
		     :rest
		     (r>))
	    n)
	   "ensure" n>
	   (choice* :name "handler-block"
		    :missing
		    (@ "nil" n>)
		    :rest
		    (r> n>))
	   "end"))
	 (:description
	  "begin ... rescue ... ensure ... end"
	  :key "bre"
	  :mark-node t
	  :shorthand calls
	  :name "nest-begin-rescue-ensure"
	  :template
	  ((save-column
	    @ "begin" n>
	    (choice* :name "expression-block"
		     :missing
		     (@ "nil")
		     :rest
		     (r>))
	    n)
	   "rescue " (p StandardError "Exception" (lambda (in)
						    (concat in " => e")))
	   n>
	   (choice* :name "handler-block"
		    :missing
		    (@ "nil" n>)
		    :rest
		    (r> n>))

	   "ensure" n>
	   (choice* :name "handler-block"
		    :missing
		    (@ "nil" n>)
		    :rest
		    (r> n>))
	   "end"))
	 (:description
	  "def ... end"
	  :key "bd"
	  :mark-node t
	  :shorthand calls
	  :name "nest-def"
	  :template
	  ("def " (p name "Name") (p none "Arguments" (lambda (in)
							(if (string= in "none")
							    ""
							  (concat "(" in ")"))))
	   n> r>))
	 (:description
	  ".each ..."
	  :key "be"
	  :mark-node t
	  :shorthand iterables
	  :name "nest-each"
	  :template
	  ((save-column r> ".each do |" (p value "Arguments") "|")
	   n>
	   @ n>
	   "end"))
	 (:description
	  ".each_with_index ..."
	  :key "bE"
	  :mark-node t
	  :shorthand iterables
	  :name "nest-each-with-index"
	  :template
	  ((save-column r> ".each_with_index do |" (p value "Arguments") "," (p index "Index") "|")
	   n>
	   @ n>
	   "end"))))
      (envelope-deindent-function #'combobulate-ruby-envelope-deindent-level)
      (indent-calculate-function #'combobulate-ruby-calculate-indent)
      (procedures-defun
       '((:activation-nodes ((:nodes ("class" "method" "module" "lambda"))))))
      (procedures-sexp
       '((:activation-nodes ((:nodes ("class" "method" "module" "lambda" "string"))))))
      (procedures-sibling
       '((:activation-nodes
	  ((:nodes
	    ((rule "keyword_parameter")
	     (rule "hash_splat_parameter")
	     ",")
	    :has-parent ("method_parameters")))
	  :selector (:match-children t))))
      (procedures-hierarchy
       '(;; For statements that can be descended into
	 (:activation-nodes
	  ((:nodes
	    ((rule "if")
	     (rule "unless")
	     (rule "while")
	     (rule "until")
	     (rule "for")
	     (rule "begin")
	     (rule "class")
	     (rule "module")
	     (rule "method")
	     "when")
	    :position at))
	  :selector (:choose node
		     :match-children (:match-rules ("block"))))
	 
	 ;; Lambda bodies
	 (:activation-nodes
	  ((:nodes
	    ((rule "lambda")
	     (rule "lambda_literal"))
	    :position at))
	  :selector (:choose node
		     :match-children (:match-rules (rule "lambda" :body))))
	 
	 ;; General case
	 (:activation-nodes
	  ((:nodes ((all)) :has-parent ((all))))
	  :selector (:choose node
		     :match-children (:discard-rules ("block")))))))))

(define-combobulate-language
 :name ruby
 :language ruby
 :major-modes (ruby-mode ruby-ts-mode)
 :custom combobulate-ruby-definitions
 :setup-fn combobulate-ruby-setup)

(defun combobulate-ruby-setup (_))

(provide 'combobulate-ruby)
;;; combobulate-ruby.el ends here
