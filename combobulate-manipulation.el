;;; combobulate-manipulation.el --- manipulate structured text with combobulate  -*- lexical-binding: t; -*-

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

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-misc)
(require 'combobulate-interface)
(require 'tempo)
(require 'map)
;;; for python-specific indent stuff
(require 'python)

(defvar combobulate-query-ring)

(declare-function combobulate--mc-place-nodes "combobulate-contrib")
(declare-function combobulate--mc-edit-nodes "combobulate-contrib")
(declare-function combobulate-envelope-expand-instructions "combobulate-envelope")
(declare-function combobulate-display-draw-node-tree "combobulate-display")
(declare-function combobulate-query-build-nested "combobulate-query")
(declare-function combobulate-query-pretty-print "combobulate-query")
(declare-function combobulate-query-ring-current-query "combobulate-query")
(declare-function combobulate-query-ring--execute "combobulate-query")
(defvar combobulate-envelope--undo-on-quit)
(defvar combobulate-key-map)

(defun combobulate--refactor-insert-copied-values (values)
  (let ((start-marker (point-marker)) (after-marker) (pt))
    ;; move this into a distinct function that can reproduce stored
    ;; input.
    (goto-char (point-marker))
    (skip-syntax-backward " ")
    (just-one-space 0)
    (setq pt (point))
    (let ((ct 0))
      (dolist (s values)
        (unless (string-blank-p s)
          (save-excursion
            (insert s)
            (setq pt (point)))
          (cl-incf ct)
          (when (= ct 1)
            (save-excursion
              (back-to-indentation)
              (setq start-marker (point-marker))))
          (goto-char pt))))
    (back-to-indentation)
    (setq after-marker (point-marker))
    (when combobulate-manipulation-indent-after-edit
      (indent-region start-marker (save-excursion
                                    (goto-char after-marker)
                                    (end-of-line)
                                    (point))))
    (goto-char start-marker)))

(defun combobulate--refactor-get-all-overlays ()
  (seq-filter
   (lambda (ov)
     (overlay-get ov 'combobulate-refactor-actions))
   (car (overlay-lists))))

(defun combobulate--refactor-clear-overlays (overlays)
  (mapc #'delete-overlay overlays))

;; (defun combobulate--refactor-divide-overlays (keep-overlays)
;;   (let ((markers))
;;     (dolist (ov-k keep-overlays)
;;       (push ov-k markers)
;;       (when (eq (overlay-get ov-k 'combobulate-refactor-actions) 'copy-region)
;;         (dolist (overlapped-ov (overlays-in (overlay-start ov-k)
;;                                             (overlay-end ov-k)))
;;           (when-let (overlapped-ov-action (overlay-get overlapped-ov 'combobulate-refactor-actions))
;;             (if (eq overlapped-ov-action 'delete-region)
;;                 (let ((start-overlaps (in-range (overlay-start overlapped-ov)
;;                                                 (overlay-start ov-k)
;;                                                 (overlay-end ov-k)))
;;                       (new-ov))
;;                   (when start-overlaps
;;                     (setq new-ov (copy-overlay overlapped-ov))
;;                     (move-overlay new-ov
;;                                   (overlay-start new-ov)
;;                                   (overlay-start ov-k))
;;                     (move-overlay overlapped-ov
;;                                   (if start-overlaps
;;                                       (overlay-end ov-k)
;;                                     (overlay-start overlapped-ov))
;;                                   (overlay-end overlapped-ov))
;;                     (push new-ov markers)))
;;               (push overlapped-ov markers))))))
;;     markers))

(defvar combobulate-refactor--copied-values nil)
(defvar combobulate-refactor--active-sessions nil
  "A list of active `combobulate-refactor' sessions.")

(defun combobulate-refactor--get-active-session (id)
  "Get the active `combobulate-refactor' session with the given ID."
  (assoc id combobulate-refactor--active-sessions))

(defun combobulate-refactor-setup (&optional id)
  "Setup a `combobulate-refactor' session with the given ID."
  (let ((id (or id (gensym "combobulate-refactor-"))))
    ;; only spawn a new session if one doesn't already exist.
    (unless (combobulate-refactor--get-active-session id)
      (push (cons id nil) combobulate-refactor--active-sessions))
    id))

(cl-defmacro combobulate-refactor ((&key (id nil)) &rest body)
  (declare (indent defun) (debug (sexp body)))
  (let ((--session (gensym))
        ;; non-nil if we're entering a session that already exists.
        (--pre-existing-session (gensym)))
    `(let ((,--pre-existing-session (combobulate-refactor--get-active-session ,id))
           (,--session (combobulate-refactor-setup ,id)))
       (cl-flet* ((add-marker (ov)
                    (push ov (alist-get ,--session combobulate-refactor--active-sessions))
                    ov)
                  (mark-range-move (beg end position)
                    (add-marker (combobulate--refactor-mark-move beg end position)))
                  (mark-range-deleted (beg end)
                    (add-marker (combobulate--refactor-mark-deleted beg end)))
                  (mark-range-highlighted (beg end &optional face advance)
                    (add-marker (combobulate--refactor-mark-highlighted beg end face advance)))
                  (mark-copy (start end)
                    (add-marker (combobulate--refactor-mark-copy start end 'combobulate-refactor--copied-values)))
                  (mark-node-copy (n)
                    (mark-copy (combobulate-node-start n) (combobulate-node-end n)))
                  (mark-range-indent (beg end target-pt baseline-column)
                    (add-marker (combobulate--refactor-mark-indent beg end target-pt baseline-column)))
                  (mark-node-deleted (n)
                    (add-marker (combobulate--refactor-mark-deleted (combobulate-node-start n)
                                                                    (combobulate-node-end n))))
                  (mark-range-label (beg end label &optional face before)
                    (add-marker (combobulate--refactor-mark-label beg end label face before)))
                  (mark-node-highlighted (n &optional face advance)
                    (mark-range-highlighted (combobulate-node-start n)
                                            (combobulate-node-end n)
                                            face
                                            advance))
                  (mark-cursor (pt)
                    (add-marker (combobulate--refactor-mark-cursor pt)))
                  (mark-node-cursor (n)
                    (mark-cursor (combobulate-node-start n)))
                  (mark-field (pt tag &optional text transformer-fn)
                    (add-marker (combobulate--refactor-mark-field
                                 pt tag (or text (symbol-name tag))
                                 transformer-fn)))
                  (update-field (tag text)
                    (seq-filter
                     (lambda (ov) (let ((actions (overlay-get ov 'combobulate-refactor-action)))
                                    (combobulate--refactor-update-field ov tag text)))
                     (alist-get ,--session combobulate-refactor--active-sessions)))
                  (mark-point (&optional pt)
                    (add-marker (combobulate--refactor-mark-position (or pt (point)))))
                  (rollback ()
                    (mapc (lambda (ov) (delete-overlay ov)) (alist-get ,--session combobulate-refactor--active-sessions))
                    (setq combobulate-refactor--active-sessions (assq-delete-all ,--session combobulate-refactor--active-sessions)))
                  (commit ()
                    (setq combobulate-refactor--copied-values nil)
                    (mapc (lambda (ov) (combobulate--refactor-commit ov t))
                          (seq-sort (lambda (a b)
                                      (and a b
                                           (overlayp a)
                                           (overlayp b)
                                           (> (or (overlay-start a) 0)
                                              (or (overlay-end b) 0))))
                                    (alist-get ,--session combobulate-refactor--active-sessions)))
                    ;; clean up.
                    (rollback)))
         (condition-case err
             (prog1 (combobulate-atomic-change-group
                      ,@body)
               (when (and (alist-get ,--session combobulate-refactor--active-sessions)
                          (not ,--pre-existing-session))
                 (error "Uncommitted changes in `%s': %s"
                        ,--session
                        (combobulate--refactor-clear-overlays
                         (combobulate--refactor-get-all-overlays)))))
           (t (rollback) (signal (car err) (cdr err))))))))


(defun combobulate-tally-nodes (nodes &optional skip-label)
  "Groups NODES into labels (if any) and their types and tallies them.

If NODES is a list of `(@label . node)' cons cells, tally nodes by that
first; followed by the node type of each grouped label."
  (if nodes
      (if (and (consp nodes) (consp (car nodes)))
          (mapconcat
           (lambda (g) (pcase-let ((`(,label . ,rest) g))
                         (let ((string-label (symbol-name label)))
                           (concat (if skip-label "" (concat (capitalize (string-trim-left string-label "@")) " "))
                                   (combobulate-tally-nodes (mapcar 'cdr rest))))))
           (combobulate-group-nodes nodes #'car) ". ")
        (string-join (mapcar (lambda (group)
                               (concat
                                (propertize (int-to-string (length (cdr group)))
                                            'face 'combobulate-active-indicator-face)
                                " "
                                (format "`%s'" (car group))))
                             (combobulate-group-nodes nodes #'combobulate-pretty-print-node-type))
                     "; "))
    "zero"))

(defun combobulate--edit-node-determine-action (arg)
  "Determine which action ARG should map to."
  (cond ((equal arg '(4)) 'after)
        ((equal arg '(16)) 'mark)
        (t 'before)))

(defun combobulate-edit-query (arg)
  "Edit clusters of nodes by query.

Uses the head of the ring `combobulate-query-ring' as the
query. If the ring is empty, then throw an error.

By default, point is placed at the start of each match. When
called with one prefix argument, place point at the end of the
matches. With two prefix arguments, mark the node instead."
  (interactive "P")
  (combobulate-query-ring--execute
   "Edit nodes matching this query?"
   "Placed cursors"
   (lambda (matches _query)
     (combobulate--mc-place-nodes matches (combobulate--edit-node-determine-action arg)))))

(defun combobulate-edit-node-siblings-dwim (arg)
  "Edit all siblings of the current node.

Combobulate will use its definition of siblings as per
\\[combobulate-navigate-next] and
\\[combobulate-navigate-previous]."
  (interactive "P")
  (with-navigation-nodes (:procedures combobulate-navigation-sibling-procedures)
    (let ((node (combobulate--get-nearest-navigable-node)))
      (combobulate--mc-edit-nodes (combobulate-nav-get-siblings node)
                                  (combobulate--edit-node-determine-action arg)
                                  (or (car-safe (combobulate-nav-get-parents node t))
                                      node)))))

(defun combobulate-edit-cluster-dwim (arg)
  "Precisely edit targeted clusters of nodes.

This looks for clusters of nodes to edit in
`combobulate-manipulation-edit-procedures'.

If you specify a prefix ARG, then the points are placed at the
end of each edited node."
  (interactive "P")
  (with-navigation-nodes (:procedures combobulate-manipulation-edit-procedures)
    (if-let ((node (combobulate--get-nearest-navigable-node)))
        (combobulate-edit-cluster
         node
         (combobulate--edit-node-determine-action arg))
      (error "Cannot find any editable clusters here"))))


(defun combobulate-edit-node-type-dwim (arg)
  "Edit nodes of the same type by node locus.

This looks for nodes of any type found in
`combobulate-navigation-default-nodes'."
  (interactive "P")
  (with-navigation-nodes (:nodes combobulate-navigation-default-nodes)
    (if-let ((node (combobulate--get-nearest-navigable-node)))
        (combobulate-edit-identical-nodes
         node (combobulate--edit-node-determine-action arg)
         (lambda (tree-node) (and (equal (combobulate-node-type node)
                                         (combobulate-node-type tree-node))
                                  (equal (combobulate-node-field-name node)
                                         (combobulate-node-field-name tree-node)))))
      (error "Cannot find any editable nodes here"))))

(defun combobulate-edit-node-by-text-dwim (arg)
  "Edit nodes with the same text by node locus.

This looks for nodes of of any type found in
`combobulate-navigation-default-nodes' that have the same text as
the node at point."
  (interactive "P")
  (if-let ((node (combobulate-node-at-point nil t)))
      (combobulate-edit-identical-nodes
       node (combobulate--edit-node-determine-action arg)
       (lambda (tree-node) (equal (combobulate-node-text tree-node)
                                  (combobulate-node-text node))))
    (error "Cannot find any editable nodes here")))

(defun combobulate-edit-identical-nodes (node action &optional match-fn)
  "Edit nodes identical to NODE if they match MATCH-FN.

The locus of editable nodes is determined by NODE's parents and
is selectable.

MATCH-FN takes one argument, a node, and should return non-nil if it is
a match."
  (let ((matches)
        ;; default to 1 "match" as there's no point in creating
        ;; multiple cursors when there's just one match
        (ct 1)
        (grouped-matches))
    (dolist (start-node (combobulate-get-parents node))
      (let ((known-ranges (make-hash-table :test #'equal :size 1024)))
        (setq matches (flatten-tree (combobulate-induce-sparse-tree
                                     start-node
                                     (lambda (tree-node)
                                       (prog1
                                           (and (funcall match-fn tree-node)
                                                (not (gethash (combobulate-node-range tree-node) known-ranges nil)))
                                         (puthash (combobulate-node-range tree-node) t known-ranges)))))))
      ;; this catches parent nodes that do not add more, new, nodes to
      ;; the editing locus by filtering them out.
      (when (> (length matches) ct)
        (setq ct (length matches))
        (push (cons start-node matches) grouped-matches)))
    (combobulate-refactor ()
      (let* ((chosen-node (combobulate-proxy-to-tree-node
                           (combobulate-proffer-choices
                            (reverse (mapcar 'car grouped-matches))
                            (lambda (_index current-node _proxy-nodes refactor-id)
                              (combobulate-refactor (:id refactor-id)
                                (rollback)
                                (mark-node-highlighted current-node)
                                (princ (format "Editing %s in %s%s\n"
                                               (combobulate-pretty-print-node-type current-node)
                                               (combobulate-proxy-to-tree-node current-node)
                                               (and (combobulate-node-field-name current-node)
                                                    (format " (%s)"
                                                            (combobulate-node-field-name current-node)))))
                                ;; rollback the outer
                                ;; `combobulate-refactor' call so
                                ;; the node cursors we place below
                                ;; are properly erased.
                                ;; place a fake cursor at every
                                ;; node to indicate where the
                                ;; matching nodes are.
                                (mapc #'mark-node-cursor
                                      (cdr (assoc (combobulate-proxy-to-tree-node node)
                                                  grouped-matches)))
                                ;; indicate the locus of editing
                                ;; by highlighting the entire node
                                ;; boundary.
                                (mark-node-highlighted current-node)))
                            :unique-only nil
                            :prompt-description
                            (format "Edit %s in"
                                    (propertize (combobulate-pretty-print-node-type node)
                                                'face 'combobulate-tree-branch-face)))))
             (matches (cdr (assoc chosen-node grouped-matches))))
        (rollback)
        (combobulate--mc-edit-nodes matches action chosen-node)))))

(defun combobulate-edit-nodes (placement-nodes)
  "Edit PLACEMENT-NODES.

PLACEMENT-NODES is a list of cons cells. The car of each cell is
the action, or the place to put the point or fake cursor. The cdr
is the node itself.

The action can be one of the following:

- `before': place the point before the node.
- `after': place the point after the node.
- `mark': mark the node."
  (let ((matches (combobulate--mc-place-nodes placement-nodes)))
    (cond ((= (length matches) 0)
           (combobulate-message "There are zero nodes available to edit."))
          (t (combobulate-message
              (concat "Editing " (combobulate-tally-nodes matches t)))))))


(defun combobulate-edit-cluster (node action)
  "Edit CLUSTER of nodes at, or around, NODE."
  (pcase-let (((cl-struct combobulate-procedure-result
                          (selected-nodes selected-nodes)
                          (parent-node parent-node))
               (or (combobulate-procedure-start node)
                   (error "No cluster to edit."))))
    (combobulate--mc-edit-nodes
     ;; Remove `@discard' matches.
     (mapcar 'cdr (seq-remove
                   ;; remove `@discard' matches. Tree-sitter does not
                   ;; return tags with `@', but Combobulate query
                   ;; search does.
                   (lambda (m) (or (equal (car m) '@discard)
                                   (equal (car m) 'discard)))
                   selected-nodes))
     action
     parent-node)))

(defun combobulate-vanish-node (&optional arg)
  "Vanishes the node at point and attempts to preserve its children."
  (interactive "^p")
  (with-argument-repetition arg
    (with-navigation-nodes (:procedures combobulate-manipulation-splicing-procedures)
      (combobulate-splice (combobulate--get-nearest-navigable-node)
                          '(before after around self)))))

(defun combobulate--vanish-node (node)
  "Vanish NODE, keeping all its children."
  (if-let* ((q (cdr (assoc (combobulate-node-type node) combobulate-manipulation-splicing-procedures)))
            (query (list q))
            (node-extent (combobulate-node-range-extent
                          (combobulate--query-from-node query node nil nil t)))
            (text (buffer-substring-no-properties (car node-extent) (cdr node-extent))))
      (combobulate--replace-node node text)
    (error "Cannot vanish node `%s'" (combobulate-pretty-print-node node))))

(defun combobulate--swap-node-regions (node-a node-b)
  "Swaps the region substring in NODE-A with NODE-B"
  (transpose-subr-1 (combobulate-node-range node-a) (combobulate-node-range node-b)))

(defun combobulate-transpose-sexps-1 (backward)
  "Capture a transposable node, either forward or BACKWARD."
  ;; do not set `:nodes' here to allow this function to work with any
  ;; node type group set in the caller.
  (with-navigation-nodes (:nodes combobulate-navigation-sexp-nodes
                                 :backward backward :skip-prefix t)
    (combobulate-forward-sexp-function-1 backward)))

(defun combobulate-transpose-sexps ()
  "Transpose sexp-like nodes around point.

If there are no legitimate sexp nodes around point, fall back to
\\[transpose-sexp]."
  (interactive)
  ;; this covers the most common case where two nodes are "adjacent"
  ;; to the point.
  (if-let ((backward-node (combobulate-transpose-sexps-1 t))
           (forward-node (combobulate-transpose-sexps-1 nil)))
      (progn
        (combobulate--goto-node forward-node t)
        (save-excursion
          (combobulate--swap-node-regions backward-node forward-node)))
    ;; for everything else, fall back to the normal (but augmented,
    ;; because `combobulate-forward-sexp-function' is used by
    ;; `transpose-sexps') to attempt a transposition by shifting
    ;; point around.
    (transpose-sexps 1 t)))

(defun combobulate-transpose-sexp-function (arg)
  ;; Here we should try to simulate the behavior of
  ;; (cons (progn (forward-sexp x) (point))
  ;;       (progn (forward-sexp (- x)) (point)))
  ;; Except that we don't want to rely on the second forward-sexp
  ;; putting us back to where we want to be, since forward-sexp-function
  ;; might do funny things like infix-precedence.
  (if (if (> arg 0)
	  (looking-at "\\sw\\|\\s_")
	(and (not (bobp))
	     (save-excursion
               (forward-char -1)
               (looking-at "\\sw\\|\\s_"))))
      ;; Jumping over a symbol.  We might be inside it, mind you.
      (progn (funcall (if (> arg 0)
			  #'skip-syntax-backward #'skip-syntax-forward)
		      "w_")
	     (cons (save-excursion (forward-sexp arg) (point)) (point)))
    ;; Otherwise, we're between sexps.  Take a step back before jumping
    ;; to make sure we'll obey the same precedence no matter which
    ;; direction we're going.
    (funcall (if (> arg 0) #'skip-syntax-backward #'skip-syntax-forward)
             " .")
    (cons (save-excursion (forward-sexp arg) (point))
	  (progn (while (or (forward-comment (if (> arg 0) 1 -1))
			    (not (zerop (funcall (if (> arg 0)
						     #'skip-syntax-forward
						   #'skip-syntax-backward)
						 ".")))))
		 (point)))))



(defun combobulate--kill-node (node)
  "Kill NODE in the current buffer."
  (and node (kill-region (combobulate-node-start node)
                         (combobulate-node-end node))))

(defun combobulate--kill-nodes (nodes)
  "Kill between the smallest and greatest range of NODES."
  (if-let (bounds (combobulate-node-range-extent nodes))
      (kill-region (car bounds) (cdr bounds))
    (error "No nodes to kill")))

(defun combobulate--mark-extent (start end &optional swap beginning-of-line)
  "Set mark to START and point to END and activates the mark.

If SWAP is non-nil, the point and mark is exchanged after, thus
reversing the order of point and mark.

If BEGINNING-OF-LINE is non-nil, then the mark is set to the
beginning of line instead of START, but only if the text is
considered whitespace by the mode's syntax table."
  (cl-macrolet
      ((change-position (f)
         `(save-excursion
            (goto-char ,f)
            (skip-chars-backward combobulate-skip-prefix-regexp
                                 (line-beginning-position))
            (when (bolp)
              (setf ,f (point))))))
    (when beginning-of-line
      (change-position start)
      (change-position end))
    (push-mark start t)
    (goto-char end)
    (activate-mark)
    (when swap
      (exchange-point-and-mark))
    (cons start end)))

(defun combobulate-extend-region-to-whole-lines (start end)
  "Extend the region between START and END to whole lines."
  (dolist (pos (list start end))
    (save-excursion
      (goto-char pos)
      (skip-chars-backward combobulate-skip-prefix-regexp
                           (line-beginning-position))
      (when (bolp)
        (setq start (point)))))
  (list start end))

(defun combobulate-node-region-lines (node)
  "Return NODE's region but made up of whole lines if possible."
  (combobulate-extend-region-to-whole-lines (combobulate-node-start node)
                                            (combobulate-node-end node)))



(defun combobulate-indent-string-first-line (text target-col)
  "Corrects the indentation of the first line of TEXT to TARGET-COL."
  (combobulate-indent-string text :first-line-operation 'absolute :first-line-amount target-col))

(defun combobulate-indent-string--strip-whitespace (s &optional count)
  "Strip S of whitespace, possibly up to COUNT whitespace characters.

If COUNT is nil, then all whitespace is stripped."
  (string-trim-left
   s (if count (rx-to-string `(** 0 ,(abs count) space))
       (rx bol (* space)))))

(defun combobulate-indent-string--count-whitespace (s)
  "Count the number of whitespace characters at the beginning of S."
  (- (length s) (length (combobulate-indent-string--strip-whitespace s))))

(defun combobulate-indent-string-1 (line operation amount)
  "Indent LINE as per OPERATION and AMOUNT.

If OPERATION is `add', then AMOUNT is added to the current
indentation.

If OPERATION is `subtract', then AMOUNT is subtracted from the
current indentation.

If OPERATION is `absolute', then AMOUNT is used as the new
indentation."
  ;; operation must be: `add', `subtract', `absolute'.
  (cl-assert (member operation '(add subtract absolute)))
  ;; see if we can invert operations if the amount warrants it.
  (cond
   ((and (eq operation 'add) (< amount 0))
    (setq operation 'subtract
          amount (- amount)))
   ((and (eq operation 'subtract) (< amount 0))
    (setq operation 'add
          amount (- amount))))
  (let ((current-indent (combobulate-indent-string--count-whitespace line))
        (line-without-indent (combobulate-indent-string--strip-whitespace line)))
    (cond ((eq operation 'add)
           ;; if the amount is zero, then there is nothing to add.
           (if (= amount 0) line
             (concat (make-string (+ current-indent amount) ? ) line-without-indent)))
          ((eq operation 'subtract)
           ;; check for whether we're subtracting more than we
           ;; have. If so, then we subtract the exact amount we need.
           (cond ((>= current-indent amount)
                  (concat (make-string (- current-indent amount) ? ) line-without-indent))
                 (t
                  (concat (make-string 0 ? ) line-without-indent))))
          ((eq operation 'absolute)
           (cl-assert (>= amount 0) "Absolute indentation must be positive.")
           (concat (make-string amount ? ) line-without-indent)))))

(cl-defun combobulate-indent-string (text &key (first-line-operation nil)
                                          (first-line-amount 0)
                                          (rest-lines-operation nil)
                                          (rest-lines-amount 0))
  "Indent TEXT specifically for the first and rest of the lines.

FIRST-LINE-OPERATION is the operation to perform on the first
line of TEXT. It can be `add', `subtract', or `absolute'.

FIRST-LINE-AMOUNT is the amount to add, subtract, or set the
first line's indentation to.

REST-LINES-OPERATION is the operation to perform on the rest of
the lines of TEXT. It can be `relative', `add', `subtract', or
`absolute'. `relative' means that the indentation of the rest of
the lines is relative to the first line's *changed*
indentation. In other words, if the first line's indentation is
changed by 2 spaces, then the rest of the lines' indentation will
be changed by 2 spaces (in the same direction) as well.

REST-LINES-AMOUNT is the amount to add, subtract, or set the rest
of the lines' indentation to. It cannot be used with `relative'."
  (cl-assert (member first-line-operation '(add subtract absolute relative nil)))
  (cl-assert (member rest-lines-operation '(relative add subtract absolute nil)))
  ;; `rest-lines-amount'  cannot be used with `relative'.
  (cl-assert (or (not (eq rest-lines-operation 'relative))
                 (and (eq rest-lines-operation 'relative)
                      (eq rest-lines-amount 0))))
  (let* ((lines (split-string text "\n"))
         ;; indentation of the first line before we modify it
         (first-line-indentation (combobulate-indent-string--count-whitespace (car lines)))
         (first-line (if first-line-operation
                         (combobulate-indent-string-1
                          (car lines)
                          (if (equal first-line-operation 'relative) 'subtract first-line-operation)
                          (if (equal first-line-operation 'relative)
                              (- first-line-indentation first-line-amount)
                            first-line-amount))
                       (car lines)))
         (new-first-line-indentation (combobulate-indent-string--count-whitespace first-line))
         (rest-lines (if rest-lines-operation
                         (mapcar (lambda (line)
                                   (if (eq rest-lines-operation 'relative)
                                       (combobulate-indent-string-1 line 'add (- new-first-line-indentation
                                                                                 first-line-indentation))
                                     (combobulate-indent-string-1 line rest-lines-operation rest-lines-amount)))
                                 (cdr lines))
                       (cdr lines))))
    (string-join (cons first-line rest-lines) "\n")))

(defun combobulate--clone-node (node position)
  "Clone NODE and place it at POSITION."
  (combobulate--place-node-or-text position node))

(defun combobulate--place-node-or-text (position node-or-text &optional mode no-trailing-newline)
  "Place NODE-OR-TEXT at POSITION.

NODE-OR-TEXT must be a valid node or a string.

If NODE-OR-TEXT is a NODE, then its first-line indentation is
calculated first and the text indented properly relative to
POSITION.

If NODE-OR-TEXT is a string, then all indentation must be handled
by the caller. The string is inserted at POSITION.

If MODE is non-nil, then it must be either `newline' or
`inline'. Where `newline' forces the placement of NODE-OR-TEXT on
a new line. If MODE is `inline' then it is places inline
alongside other nodes around POSITION.

If NO-TRAILING-NEWLINE is non-nil, then no trailing newline is inserted
after NODE-OR-TEXT."
  (let* ((col (save-excursion (goto-char position)
                              (current-column)))
         (node-text)
         (newline-insert (and (or (eq mode 'newline) (null mode))
                              (combobulate-before-point-blank-p position))))
    (cond
     ((stringp node-or-text)
      (setq node-text (combobulate-indent-string
                       node-or-text
                       :first-line-amount col
                       :first-line-operation 'absolute
                       :rest-lines-amount 'relative)))
     ((or (combobulate-node-p node-or-text)
          (combobulate-proxy-node-p node-or-text))
      (let ((sequence-separator
             ;; attempt to guess the sequence separator, almost always
             ;; an anonymous node, that (usually!) follows nodes --
             ;; think array/list separators; semicolons after
             ;; expressions, etc. -- and ensure it is appended to the
             ;; cloned text. This is of course not the proper way to
             ;; do this: the `xxx-grammar.json' files contain the
             ;; rules for sequences and the character to use. This is
             ;; merely the 90% solution that works for most, but not
             ;; all, instances.
             (save-excursion
               (combobulate--goto-node node-or-text t)
               (let ((node-after (combobulate-node-on (point) (1+ (point)) nil nil)))
                 ;; we only care about unnamed nodes: yes, it's
                 ;; possible named nodes are used for sequence
                 ;; separators, I suppose, but this is not handled
                 ;; here at all.
                 (if (combobulate-node-named-p node-after)
                     ""
                   (combobulate-node-text node-after)))))
            (node-col (save-excursion
                        (combobulate--goto-node node-or-text)
                        (current-indentation))))
        (setq node-text (if newline-insert
                            (combobulate-indent-string
                             ;; the first line may not include all of its
                             ;; indentation because the node extents won't
                             ;; include it. This fixes it so it does.
                             (combobulate-indent-string-first-line
                              (concat (combobulate-node-text node-or-text) sequence-separator)
                              node-col)
                             :first-line-amount (- col node-col)
                             :first-line-operation 'add)
                          (concat (combobulate-node-text node-or-text) sequence-separator)))))
     (t (error "Cannot place node or text `%s'" node-or-text)))
    (goto-char position)
    ;; If a node has nothing but whitespace preceding it, then it's a
    ;; "newline-delimited" node. Newline-delimited nodes are regular
    ;; nodes that exist, usually, on a line of their own. That is on
    ;; contrast to inline-delimited nodes that are placed to the left
    ;; or right on a line with other nodes.
    (if newline-insert
        ;; newline-delimited node
        (progn
          (unless no-trailing-newline (split-line 0))
          (combobulate--refactor-insert-copied-values
           (list node-text)))
      ;; inline-delimited node
      (save-excursion
        (insert node-text)
        (just-one-space 0)
        (unless (looking-at "\\s-")
          (insert " ")))
      (just-one-space))))

(defun combobulate--mark-node (node &optional swap beginning-of-line)
  "Mark NODE in the current buffer.

See `combobulate--mark-extent' for argument explanations."
  (when node
    (combobulate--mark-extent (combobulate-node-start node)
                              (combobulate-node-end node)
                              swap
                              beginning-of-line)))

(defun combobulate--delete-text (beg end &optional correct-indentation delete-blank-lines)
  (save-excursion
    (let ((text (save-excursion
                  (prog1 (delete-and-extract-region beg end)))))
      (prog1 (if correct-indentation
                 (combobulate-indent-string-first-line
                  text
                  (save-excursion
                    (goto-char beg)
                    (current-indentation)))
               text)
        (save-excursion
          ;; Only trim lines if there is more than one as
          ;; `delete-blank-lines', in case there is only one, will
          ;; delete that blank line. That can have very bad
          ;; repercussions if the deletion is used as part of a
          ;; greater chain of operations, like moving text in a
          ;; refactor
          (if (= (combobulate-string-count "\n" text) 0)
              (save-restriction
                (goto-char end)
                (narrow-to-region (line-beginning-position) (point-max))
                (when (and delete-blank-lines (or (> (combobulate-count-lines-ahead (point)) 1)
                                                  (= (combobulate-string-count "\n" text) 0)))
                  (delete-blank-lines)))
            (save-restriction
              (narrow-to-region (line-beginning-position) (point-max))
              (when (and delete-blank-lines (or (> (combobulate-count-lines-ahead (point)) 1)
                                                (= (combobulate-string-count "\n" text) 0)))
                (delete-blank-lines)))))))))

(defun combobulate--delete-node (node &optional correct-indentation delete-blank-lines)
  "Deletes NODE in the current buffer and returns its text.

If CORRECT-INDENTATION is non-nil, then the node's first-line
indentation is set according to its current indentation in the
buffer.

If DELETE-BLANK-LINES is non-nil, then all blank lines left behind by
the deleted node are removed."
  (when node
    (combobulate--delete-text (combobulate-node-start node)
                              (combobulate-node-end node)
                              correct-indentation
                              delete-blank-lines)))

(defun combobulate--replace-node (node text &optional before)
  "Replace NODE with TEXT and maybe place point BEFORE.

The NODE is deleted (`delete-region') and TEXT inserted in its place.

If BEFORE then point is placed before the text and not after.

The variable `combobulate-manipulation-indent-method' determines
how indentation of the TEXT happens.  This is mostly of interest
in Python where whitespace is very important, and automatic
indentation unreliable."
  (when node
    (atomic-change-group
      (let ((node-start (combobulate-node-start node)))
        (combobulate--goto-node node)
        (combobulate--delete-node node)
        (let ((beg) (end)
              (pt (point))
              (offset 0))
          (setq beg (pos-bol))
          (save-excursion (beginning-of-line) (insert text))
          (setq end (pos-eol))
          (goto-char node-start)
          (cond ((eq 'mode combobulate-manipulation-indent-method)
                 (indent-region beg end))
                ((eq 'first combobulate-manipulation-indent-method)
                 (setq offset (save-excursion
                                (goto-char pt)
                                (skip-syntax-forward " ")
                                (current-column)))
                 (indent-rigidly beg end (- (current-column) offset))))
          (when before
            (goto-char pt)))))
    (combobulate-delete-empty-lines)
    (combobulate-delete-whitespace)
    (when (eq 'mode combobulate-manipulation-indent-method)
      (indent-according-to-mode))))

(defun combobulate-kill-node-dwim (&optional arg)
  "Kill the most likely node on or near point ARG times.

The exact node that is killed will depend on the location of
point relative to the nodes in
`combobulate-navigation-default-nodes'."
  (interactive "p")
  (with-argument-repetition arg
    (with-navigation-nodes (:skip-prefix t :procedures combobulate-navigation-sibling-procedures)
      (when-let ((node (combobulate-nav-get-self-sibling (combobulate--get-nearest-navigable-node))))
        (combobulate-message "Killed" node)
        (combobulate--kill-node node)))))

(defvar-keymap combobulate-proffer-map
  :parent nil
  :doc "Keymap for `combobulate-proffer-choices'.

You can bind regular commands to keys like a normal map, or you
can bind one of the following special symbols:

The symbol `done' will accept the current choice and exit. The
symbol `next' will move to the next choice. The symbol `prev'
will move to the previous choice. The symbol `cancel' will cancel
the current choice and exit."
  "TAB" 'next
  "S-<tab>" 'prev
  "<backtab>" 'prev
  "RET" 'done
  "C-g" 'cancel
  "C-l" 'recenter
  "M-c" 'recursive-edit)

(cl-defun combobulate-proffer-action-highlighter (_index current-node _proxy-nodes refactor-id)
  "Helper for `combobulate-proffer-choices' that highlights NODE."
  (combobulate-refactor (:id refactor-id)
    (mark-node-highlighted current-node)))

(cl-defun combobulate-proffer-choices (nodes action-fn &key (first-choice nil)
                                             (reset-point-on-abort t) (reset-point-on-accept nil)
                                             (prompt-description nil)
                                             (extra-map nil)
                                             (flash-node nil)
                                             (accept-action 'rollback)
                                             (cancel-action 'commit)
                                             (switch-action 'rollback)
                                             (allow-numeric-selection nil)
                                             (signal-on-abort nil)
                                             (start-index 0)
                                             (before-switch-fn nil)
                                             (after-switch-fn nil)
                                             (unique-only t))
  "Interactively browse NODES one at a time with ACTION-FN applied to it.

Interactively let the user select one of the nodes in NODES and
preview the transformation Combobulate would apply if they accept
that choice. The active node is rendered with ACTION-FN to allow
the user to see what they are selecting and the possible
transformation that will take place if they accept the choice.

The user can then select the node with RET, or abort the
selection with C-g. The user can also cycle through the nodes
with TAB and S-TAB. See `combobulate-proffer-map'.

If there is exactly one node in NODES, then it is automatically
selected and no user interaction is required.

ACTION-FN must be a function that takes four arguments:

   (INDEX CURRENT-NODE PROXY-NODES REFACTOR-ID)

Where INDEX is the current index of the node in PROXY-NODES,
which is the same as CURRENT-NODE. PROXY-NODES is a list of proxy
nodes available to the user to choose from. REFACTOR-ID is a
unique identifier for the current refactoring operation. Use
`combobulate-refactor' with the REFACTOR-ID to manipulate the
state of the refactoring operation.

Setting `:first-choice' to non-nil prevents the system from
proffering choices at all; instead, the first choice is
automatically picked, if there is a choice to make.

When `:reset-point-on-abort' or `:reset-point-on-accept' is
non-nil, the point is reset to where it was when the proffer was
first started depending on the outcome of the proffer.

If `:flash-node' is non-nil, then display a node tree in the echo
area alongside the status message.

If non-nil, `:unique-only' filters out duplicate nodes *and*
nodes that share the same range extent. I.e., a `block' and a
`statement' node that effectively encompass the same range in the
buffer.

`:allow-numeric-selection' is a boolean that determines whether
the user can select a node by typing its index. If non-nil, then
the user can type a number to select the node at that index from
1 through to 9.

`:extra-map' is a list of cons cells consisting of (KEY
. COMMAND). The extra keys are mapped into the proffer map,
`combobulate-proffer-map'.

`:accept-action' is a symbol that determines what happens when
the user accepts a choice. The following symbols are supported:

  `rollback' - Rollback the refactoring operation.
  `commit' - Commit the refactoring operation.

`:cancel-action' and `:switch-action' is the same as
`:accept-action', but for when the user cancels the choice or
interactively switches to a different node.

`:signal-on-abort' is a symbol that determines what happens when
the user aborts the choice. The following symbols are supported:

  `error' - Signal an error.
  `message' - Display a message in the echo area.

`:prompt-description' is a string that is displayed in the prompt.

`:start-index' is an integer that determines the starting index
of the node in the list of nodes. This is useful when you want to
skip over the first few nodes in the list."
  (setq allow-numeric-selection
	;; numeric selection uses `C-1' through to `C-9' which cannot
	;; always be typed on a terminal.
	(and allow-numeric-selection
             (display-graphic-p)
             combobulate-proffer-allow-numeric-selection))
  (let ((proxy-nodes
         (and nodes
              (funcall #'combobulate-make-proxy
                       ;; strip out duplicate nodes. That
                       ;; includes nodes that are duplicates
                       ;; of one another; however, we also
                       ;; strip out nodes that share the
                       ;; same range extent.
                       (if unique-only
                           (seq-uniq nodes (lambda (a b)
                                             (or (equal a b)
                                                 (equal (combobulate-node-range a)
                                                        (combobulate-node-range b)))))
                         nodes))))
        (result) (state 'continue) (current-node)
        (index start-index) (pt (point)) (raw-event)
        (refactor-id (combobulate-refactor-setup))
        (change-group)
        (map (let ((map (make-sparse-keymap)))
               (set-keymap-parent map combobulate-proffer-map)
               (when extra-map
                 (define-key map (this-command-keys) 'next)
                 (mapc (lambda (k) (define-key map (car k) (cdr k))) extra-map))
               (when allow-numeric-selection
                 (dotimes (i 9)
                   (define-key map (kbd (format "C-%d" (1+ i))) (1+ i))))
               map)))
    (if proxy-nodes
        (progn
          (cl-assert (< start-index (length proxy-nodes)) nil
                     "Start index %d is greater than the number of nodes %d"
                     start-index (length proxy-nodes))
          (condition-case err
              (with-undo-amalgamate
                (catch 'exit
                  (while (eq state 'continue)
                    (unless change-group
                      (setq change-group (prepare-change-group)))
                    (catch 'next
                      (setq current-node (nth index proxy-nodes))
                      (combobulate-refactor (:id refactor-id)
                        (cl-flet ((refactor-action (action)
                                    (cond ((eq action 'commit)
                                           (commit))
                                          ((eq action 'rollback)
                                           (rollback))
                                          (t (error "Unknown action: %s" action)))))
                          (and before-switch-fn (funcall before-switch-fn))
                          (funcall action-fn index current-node proxy-nodes refactor-id)
                          ;; if we have just one item, or if
                          ;; `:first-choice' is non-nil, we pick the first
                          ;; item in `proxy-nodes'
                          (if (or (= (length proxy-nodes) 1) first-choice)
                              (progn (refactor-action accept-action)
                                     (setq state 'accept))
                            (setq result
                                  (condition-case nil
                                      (lookup-key
                                       map
                                       ;; we need to preserve the raw
                                       ;; event so we can put it back on
                                       ;; the unread event loop later if
                                       ;; the key is not recognised.
                                       (setq raw-event
                                             (read-key-sequence-vector
                                              (substitute-command-keys
                                               (format "%s %s`%s': `%s' or \\`S-TAB' to cycle%s; \\`C-g' quits; rest accepts.%s"
                                                       (combobulate-display-indicator index (length proxy-nodes))
                                                       (concat (or prompt-description "")
                                                               (and prompt-description (propertize " → " 'face 'shadow)))
                                                       (propertize (combobulate-pretty-print-node current-node) 'face
                                                                   'combobulate-tree-highlighted-node-face)
                                                       (mapconcat (lambda (k)
                                                                    (propertize (key-description k) 'face 'help-key-binding))
                                                                  ;; messy; is this really the best way?
                                                                  (where-is-internal 'next map)
                                                                  ", ")
                                                       (if allow-numeric-selection (concat "; \\`C-1' to \\`C-9' to select") "")
                                                       (if (and flash-node combobulate-flash-node)
                                                           (concat "\n"
                                                                   (or (combobulate-display-draw-node-tree
                                                                        (combobulate-proxy-to-tree-node current-node))
                                                                       ""))
                                                         ""))))))
                                    ;; if `condition-case' traps a quit
                                    ;; error, then map it into the symbol
                                    ;; `cancel', which corresponds to the
                                    ;; equivalent event in the state
                                    ;; machine below.
                                    (quit 'cancel)
                                    (t (rollback) 'cancel)))
                            (and after-switch-fn (funcall after-switch-fn))
                            (pcase result
                              ('prev
                               (refactor-action switch-action)
                               (setq index (mod (1- index) (length proxy-nodes)))
                               (throw 'next nil))
                              ('next
                               (refactor-action switch-action)
                               (setq index (mod (1+ index) (length proxy-nodes)))
                               (throw 'next nil))
                              ('done
                               (refactor-action accept-action)
                               (combobulate-message "Committing" current-node)
                               (setq state 'accept))
                              ((pred functionp)
                               (funcall result)
                               (refactor-action accept-action)
                               (throw 'next nil))
                              ('cancel
                               (combobulate-message "Cancelling...")
                               (setq state 'abort)
                               (refactor-action cancel-action)
                               (keyboard-quit))
                              ;; handle numeric selection `1' to `9'
                              ((and (pred (numberp)) (pred (lambda (n) (and (>= n 1)
                                                                            (<= n 9)
                                                                            (<= n (length proxy-nodes)))))
                                    n)
                               (refactor-action switch-action)
                               (setq index (1- n))
                               (throw 'next nil))
                              (_
                               (combobulate-message "Committing...")
                               ;; pushing `raw-event' to
                               ;; `unread-command-events' allows for a
                               ;; seamless exit out of the proffer
                               ;; prompt by preserving the the last,
                               ;; unhandled event the user inputted.
                               (when (length> raw-event 0)
                                 (push (aref raw-event 0) unread-command-events))
                               (refactor-action accept-action)
                               (setq state 'accept)))))))
                    (when change-group
                      (cancel-change-group change-group))
                    (activate-change-group change-group))))
            (quit (when signal-on-abort
                    (signal (car err) (cdr err))))))
      (error "There are no choices to make"))
    ;; Determine where point is placed on exit and whether we return
    ;; the current node or not.
    (cond
     ((and (eq state 'abort))
      (when reset-point-on-abort (goto-char pt))
      nil)
     ((and (eq state 'accept))
      (when reset-point-on-accept
        (goto-char pt))
      current-node)
     (t (error "Unknown termination state `%s'" state)))))

(defun combobulate-clone-node-dwim (&optional arg)
  "Clone node at point ARG times."
  (interactive "^p")
  (with-argument-repetition arg
    (with-navigation-nodes (:procedures combobulate-navigation-sibling-procedures)
      (when-let ((node (combobulate-proffer-choices
                        (seq-sort #'combobulate-node-larger-than-node-p (combobulate--get-all-navigable-nodes-at-point))
                        (lambda (_index current-node _proxy-nodes refactor-id)
                          (combobulate-refactor (:id refactor-id)
                            (mark-node-deleted current-node)
                            (mark-node-highlighted current-node)
                            (combobulate-move-to-node current-node)
                            (combobulate-skip-whitespace-forward)))
                        :reset-point-on-abort t)))
        (combobulate-message "Cloning" node)
        (combobulate--clone-node node (combobulate-node-start node))))))



(defun combobulate-mark-node-at-point (&optional arg beginning-of-line)
  "Mark the most likely node on or near point ARG times.

The exact node that is marked will depend on the location of
point relative to the nodes in
`combobulate-navigation-default-nodes'."
  (interactive "^p")
  (with-argument-repetition arg
    ;; if the mark's ahead of point then we're at the beginning of the
    ;; region; if so, swap places.
    (when (and mark-active (> (point) (mark)))
      (exchange-point-and-mark))
    (when-let ((parent (car (seq-drop-while
                             #'combobulate-node-in-region-p
                             (combobulate-nav-get-parents (combobulate-node-at-point))))))
      (combobulate--mark-node parent nil beginning-of-line)
      (combobulate--flash-node parent)
      parent)))

(defun combobulate-mark-node-dwim (&optional arg beginning-of-line first-choice)
  "Mark the most likely node on or near point ARG times.

The exact node that is marked will depend on the location of
point relative to the nodes in
`combobulate-navigation-default-nodes'.

If BEGINNING-OF-LINE is non-nil, then the marked node has its point and
mark extended, if possible, to the whole line.

Setting FIRST-CHOICE to non-nil disables proffered choices if there is
more than one."
  (interactive "^p")
  (with-argument-repetition arg
    ;; if the mark's ahead of point then we're at the beginning of the
    ;; region; if so, swap places.
    (when (and mark-active (> (point) (mark)))
      (exchange-point-and-mark))
    (let ((nodes (cons (combobulate--get-nearest-navigable-node)
                       (combobulate-nav-get-parents
                        (combobulate-node-at-point)))))
      ;; maybe mark the thing at point first even though it may (or may
      ;; not) match the extents of a node.
      ;;
      ;; Because we shift point around all the time, we don't want the
      ;; `thing-at-point' machinery to pick up stray "things" at the
      ;; edges of our region; that would be confusing. So only apply
      ;; it if we don't have a region (i.e., the first time we run
      ;; this command)
      (when (and combobulate-mark-node-or-thing-at-point (not (use-region-p)))
        ;; NOTE: this may need refinement over time; for now we
        ;; hardcode the `:type' to be the symbol name of the thing
        ;; we're looking for, though obviously that is most likely a
        ;; made-up node type.
        (when-let ((bounds (bounds-of-thing-at-point combobulate-mark-node-or-thing-at-point))
                   (thing (thing-at-point combobulate-mark-node-or-thing-at-point)))
          (setq nodes (cons (make-combobulate-proxy-node
                             :start (car bounds)
                             :end (cdr bounds)
                             :type (symbol-name combobulate-mark-node-or-thing-at-point)
                             :named nil
                             :node nil
                             :field nil
                             :pp (capitalize (symbol-name combobulate-mark-node-or-thing-at-point))
                             :text thing)
                            nodes))))
      (combobulate-proffer-choices
       (seq-drop-while #'combobulate-node-in-region-p nodes)
       (lambda (index current-node proxy-nodes refactor-id)
         (combobulate-refactor (:id refactor-id)
           ;; highlight the current node so the user can see the
           ;; extent of the region.
           (combobulate--mark-node current-node t beginning-of-line)
           ;; also, if it exists, mark the *next* node in the list
           ;; with a highlight outline
           (when-let (next-node (nth (1+ index) proxy-nodes))
             (mark-node-highlighted next-node))
           ;; if the user enabled numeric selection, then label the
           ;; first ten nodes.
           (when (and (display-graphic-p) combobulate-proffer-allow-numeric-selection)
             (dotimes (i 9)
               (when-let ((idx-node (nth i proxy-nodes)))
                 (mark-range-label
                  (combobulate-node-start idx-node)
                  (1+ (combobulate-node-start idx-node))
                  (int-to-string (1+ i))))))))
       ;; this feature only works properly on displays that support
       ;; ctrl-<number> keys.
       :allow-numeric-selection (display-graphic-p)
       :reset-point-on-abort t
       :cancel-action 'rollback
       :reset-point-on-accept nil
       ;; This allows repetition of the command that
       ;; `combobulate-mark-node-dwim' is bound to, but this should be
       ;; built into `combobulate-proffer-choices'. Furthermore, is
       ;; `where-is-internal' really the best way to do this?
       :extra-map (append
                   (mapcar (lambda (key) (cons key 'next))
                           (where-is-internal #'combobulate-mark-node-dwim
                                              combobulate-key-map))
                   ;; `M-h' (default) will expand the region; `M-H'
                   ;; contracts it.
                   (list (cons (kbd "M-H") 'prev)))
       :flash-node t
       :first-choice first-choice))))

(defun combobulate-mark-defun (&optional arg)
  "Mark defun and place point at the end ARG times.

Uses `combobulate-navigation-defun-nodes' to determine what a
defun is.  Repeat calls expands the scope."
  (interactive "p")
  (with-argument-repetition arg
    (with-navigation-nodes (:nodes combobulate-navigation-defun-nodes :skip-prefix t)
      (unless (combobulate-mark-node-at-point nil t)
        (if (< arg 0)
            (combobulate-navigate-beginning-of-defun)
          (when (and mark-active (> (mark) (point)))
            (exchange-point-and-mark))
          (combobulate-navigate-end-of-defun))))))

(defun combobulate--partition-by-position (self-node query-nodes)
  "Given QUERY-NODES group them relative to SELF-NODE"
  (mapcar (lambda (query-node)
            (let ((node query-node))
              (cond
               ((combobulate-node-before-node-p node self-node)
                (cons 'before node))
               ((combobulate-node-eq node self-node)
                (cons 'self node))
               ((combobulate-node-after-node-p node self-node)
                (cons 'after node))
               ((combobulate-node-contains-node-p self-node node)
                (cons 'around node))
               (t (error "not in partition %s" query-node)))))
          query-nodes))

(defun combobulate-splice-up (&optional arg)
  (interactive "^p")
  (with-argument-repetition arg
    (with-navigation-nodes (:procedures combobulate-manipulation-splicing-procedures)
      (combobulate-splice (combobulate--get-nearest-navigable-node) '(self after around)))))

(defun combobulate-splice-down (&optional arg)
  (interactive "^p")
  (with-argument-repetition arg
    (with-navigation-nodes (:procedures combobulate-manipulation-splicing-procedures)
      (combobulate-splice (combobulate--get-nearest-navigable-node) '(self before around)))))

(defvar combobulate-refactor--copied-values nil)

(defun combobulate-splice (point-node partitions &optional matches)
  "Splice POINT-NODE by PARTITIONS.

PARTITIONS must be an alist of partitions, possibly generated by
`combobulate--partition-by-position'.

Each member of PARTITIONS must be one of:

 `before', to preserve things before the POINT-NODE;
 `after', to preserve things after the POINT-NODE;
 `around' to preserve nodes larger than POINT-NODE;
 `self' to preserve POINT-NODE."
  (let* ((procedure (combobulate-procedure-result-selected-nodes
                     (combobulate-procedure-start point-node)))
         (baseline-target nil)
         (tally)
         (matches (or matches (cdr procedure))))
    (combobulate-refactor ()
      (pcase-dolist (`(,action . ,node) matches)
        (pcase action
          ('@discard
           (mark-node-deleted node)
           (setq baseline-target (combobulate-baseline-indentation node))
           (push (cons action node) tally))
          ('@keep
           (pcase-dolist (`(,position . ,part-node)
                          (combobulate--partition-by-position point-node (list node)))
             (when (member position partitions)
               (push (cons action node) tally)
               (pcase-let ((`(,start ,end) (combobulate-extend-region-to-whole-lines
                                            (combobulate-node-start part-node)
                                            (combobulate-node-end part-node))))
                 (mark-copy start end)
                 (mark-range-indent start end
                                    baseline-target
                                    (combobulate-baseline-indentation part-node))))))))
      (combobulate-message "Spliced." (combobulate-tally-nodes tally nil))
      (commit))
    (combobulate--refactor-insert-copied-values combobulate-refactor--copied-values)))

(defun combobulate-move-past-close-and-reindent (&optional arg)
  (interactive "^p")
  (with-argument-repetition arg
    (combobulate-move-to-node (combobulate--navigate-up))
    (if-let (sibling (combobulate--navigate-next))
        (progn
          (combobulate-move-to-node sibling)
          (split-line))
      (let ((col (current-column)))
        (combobulate-move-to-node (combobulate--navigate-self-end) t)
        (newline)
        (indent-to col)))))

(defun combobulate--yeet (point-node)
  (let* ((source-node (or (car (reverse (combobulate-nav-get-siblings point-node)))
                          (error "No valid sibling node.")))
         (target-node
          (save-excursion
            (combobulate-move-to-node
             (with-navigation-nodes (:nodes combobulate-navigation-parent-child-procedures)
               (combobulate-nav-get-parent point-node)))
            (or (combobulate-make-proxy (combobulate--get-sibling
                                         (combobulate-node-at-point)
                                         'forward))
                (error "No valid sibling node.")))))
    (save-excursion
      (let ((pos))
        (combobulate-refactor ()
          (mark-range-move
           (combobulate-node-start source-node)
           (combobulate-node-end source-node)
           (progn
             (combobulate-move-to-node target-node)
             (back-to-indentation)
             (split-line 1)
             (setq pos (point-marker))))
          (commit))
        (delete-blank-lines)
        (goto-char pos)
        (delete-blank-lines)))))

(defun combobulate--yoink (point-node)
  (let* ((point-sibling (car (reverse (combobulate-nav-get-siblings point-node))))
         (target-node
          (save-excursion
            (combobulate-move-to-node
             (with-navigation-nodes (:nodes combobulate-navigation-parent-child-procedures)
               (combobulate-nav-get-parent point-node)))
            (or (combobulate-make-proxy (combobulate--get-sibling
                                         (combobulate-node-at-point)
                                         'forward))
                (error "No valid sibling node.")))))
    (save-excursion
      (let ((pos-col))
        (combobulate-refactor ()
          (mark-range-move
           (combobulate-node-start target-node)
           (combobulate-node-end target-node)
           (progn
             (setq pos-col (current-indentation))
             ;; find the right place to place the newline and indent.
             (if point-sibling
                 (combobulate-move-to-node point-sibling t)
               (end-of-line))
             (when (combobulate-after-point-blank-p (point))
               (newline)
               (indent-to pos-col))
             (point)))
          (commit))))))

(defun combobulate-yoink-forward (arg)
  (interactive "^p")
  (with-argument-repetition arg
    (with-navigation-nodes (:procedures combobulate-navigation-sibling-procedures)
      (combobulate--yoink (combobulate--get-nearest-navigable-node)))))

(defun combobulate-yeet-forward (arg)
  (interactive "^p")
  (with-argument-repetition arg
    (with-navigation-nodes (:procedures combobulate-navigation-sibling-procedures)
      (combobulate--yeet (combobulate--get-nearest-navigable-node)))))

(defun combobulate-delete-whitespace ()
  "Maybe deletes excess whitespace around point.

Whether this function does anything or not depends on
`combobulate-manipulation-trim-whitespace'."
  (cond ((eq combobulate-manipulation-trim-whitespace 'backward)
         (delete-horizontal-space t))
        ((eq combobulate-manipulation-trim-whitespace 'all)
         (delete-horizontal-space))))


(defun combobulate-delete-empty-lines ()
  "Delete empty lines around point"
  (when combobulate-manipulation-trim-empty-lines
    (delete-blank-lines)))

(defun combobulate--drag (direction)
  "Perform a drag operation on the current navigation node in DIRECTION.

If the current node has no siblings in the specified direction,
an error is raised. If the operation is successful, the cursor is
moved to the modified node."
  (let* ((up (eq direction 'up))
         (node (or (combobulate--get-nearest-navigable-node) (error "No navigable node")))
         (sibling (combobulate--get-sibling node (if up 'backward 'forward)))
         (self (combobulate--get-sibling node 'self)))
    (unless sibling
      (error "No sibling node to swap with in that direction"))
    (combobulate--goto-node sibling)
    (save-excursion (combobulate--swap-node-regions self sibling))
    (combobulate--get-sibling (combobulate--get-nearest-navigable-node) 'self)))

(defun combobulate-baseline-indentation-default (pos)
  (save-excursion
    (goto-char pos)
    (current-indentation)))

(defun combobulate-baseline-indentation (node-or-pos)
  "Determine the baseline column offset of NODE-OR-POS."
  (funcall combobulate-calculate-indent-function
           (cond ((combobulate-node-p node-or-pos) (combobulate-node-start node-or-pos))
                 ((or (markerp node-or-pos)
                      (integerp node-or-pos))
                  node-or-pos)
                 (t (error "Unknown node-or-pos `%s'" node-or-pos)))))

(defun combobulate-indent-node (node column &optional relative)
  "Indent NODE to COLUMN.

This function rigidly indents NODE by COLUMN while
preserving the relative indentation of each line.

If RELATIVE is non-nil, then COLUMN adds or subtracts from the
baseline indentation (as calculated by
`combobulate-baseline-indentation') instead of absolutely
indenting to that column."
  (let ((baseline-column (combobulate-baseline-indentation node)))
    (combobulate--mark-node node t t)
    (combobulate-indent-region (point) (mark)
                               baseline-column
                               (if relative column
                                 (- column baseline-column)))))

(defun combobulate-indent-region (start end column &optional baseline-column relative)
  "Indent the region between START and END to COLUMN.

If RELATIVE is non-nil, then add or subtract from the current
indentation rather than setting the absolute indentation from the
beginning of the line."
  (let ((baseline-column
         (or baseline-column
             (combobulate-baseline-indentation start))))
    (indent-rigidly start end (if relative column (- column baseline-column)))))

(defun combobulate-take-query-nodes-between (nodes start-label stop-label)
  "Keeps all NODES starting from START-LABEL and ending at STOP-LABEL."
  (let ((filtered) (collect))
    (nreverse (progn
                (catch 'done
                  (pcase-dolist (`(,label . ,node) nodes)
                    (when (and (eq stop-label label) collect)
                      (throw 'done filtered))
                    (when (eq start-label label)
                      (setq collect t))
                    (when collect
                      (push (cons label node) filtered))))
                filtered))))

(defun combobulate--refactor-commit (ov &optional destroy-overlay)
  (if-let ((actions (overlay-get ov 'combobulate-refactor-actions)))
      (let* ((buf (overlay-buffer ov))
             (ov-start (overlay-start ov))
             (ov-end (overlay-end ov)))
        (when buf
          (dolist (action actions)
            (when combobulate-debug
              (princ (format "action: %s (ov %s)\n" action ov)))
            (pcase action
              (`(copy-region ,target-var)
               (push (buffer-substring-no-properties
                      (max (point-min) ov-start)
                      (min (point-max) (save-excursion
                                         (goto-char ov-end)
                                         (cond
                                          ((looking-at "\n") (1+ ov-end))
                                          ((looking-at " ") (+
                                                             (- (match-end 0)
                                                                (match-beginning 0))
                                                             ov-end))
                                          (t ov-end)))))
                     (symbol-value target-var)))
              (`(move ,beg ,end ,position)
               (let ((pos position)
                     (trailing-newlines (combobulate-count-lines-ahead end)))
                 (save-excursion
                   (goto-char position)
                   (combobulate--place-node-or-text
                    position
                    (prog1 (combobulate--delete-text beg end t t)
                      (goto-char pos))
                    nil
                    (> trailing-newlines 1)))))
              ('(delete-region)
               ;; detect single-char overlays with newlines.
               (unless (string-blank-p (buffer-substring ov-start ov-end))
                 (delete-region ov-start ov-end)))
              (`(indent ,pt ,baseline-column)
               (combobulate-indent-region ov-start ov-end pt baseline-column))
              ('(set-point)
               (goto-char ov-start))
              (`(field . ,_))
              ;; do nothing - their action is part of the overlay
              ('(highlighted))
              ('(labelled))
              ('(cursor))
              (_ (error "Unknown refactor commit action `%s'" action)))))
        (when destroy-overlay
          (delete-overlay ov)))
    (error "Overlay `%s' does not have a `combobulate-refactor-action's property" ov)))

(defun combobulate--refactor-mark-position (pt)
  (let ((ov (make-overlay pt pt)))
    (overlay-put ov 'combobulate-refactor-actions '((set-point)))
    ov))

(defun combobulate--refactor-mark-move (beg end target-position)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'combobulate-refactor-actions `((move ,beg ,end ,target-position)))
    (when combobulate-debug
      (overlay-put ov 'face 'smerge-refined-changed))
    ov))

(defun combobulate--refactor-mark-deleted (beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'combobulate-refactor-actions '((delete-region)))
    (when combobulate-debug
      (overlay-put ov 'face 'smerge-refined-removed))
    ov))

(defun combobulate--refactor-mark-highlighted (beg end &optional face advance)
  (let ((ov (make-overlay beg end nil advance advance)))
    (overlay-put ov 'combobulate-refactor-actions '((highlighted)))
    (overlay-put ov 'face (or face 'combobulate-refactor-highlight-face))
    ov))

(defun combobulate--refactor-mark-label (beg end label &optional face before)
  "Mark the region between BEG and END with LABEL.

If BEFORE is non-nil, then the label is placed (using the special
`before' overlay property) before the region."
  (let ((ov (make-overlay beg end nil nil nil)))
    (overlay-put ov 'combobulate-refactor-actions '((labelled)))
    (overlay-put ov 'face (or face 'combobulate-refactor-label-face))
    (if before
        (overlay-put ov 'before-string label)
      (overlay-put ov 'display label))
    ov))

(defun combobulate--refactor-mark-copy (beg end &optional target-var)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'combobulate-refactor-actions `((copy-region ,target-var)))
    (when combobulate-debug
      (overlay-put ov 'face 'diff-added))
    ov))

(defun combobulate--refactor-mark-indent (beg end pt baseline-column)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'combobulate-refactor-actions `((indent ,pt ,baseline-column)))
    (when combobulate-debug
      (overlay-put ov 'face 'diff-changed))
    ov))

(defun combobulate--refactor-mark-field (pt tag text &optional transformer-fn)
  ;; check if there is already a field overlay at this position with `tag' at `pt':
  (if-let (ov (seq-find (lambda (ov)
                          (combobulate--refactor-field-has-tag-p ov tag 'field))
                        (overlays-at pt)))
      ov
    (let ((ov (make-overlay pt pt nil t nil)))
      ;; args 2 and 3 refer to respectively `tag' and the default value.
      (overlay-put ov 'combobulate-refactor-actions `((field ,tag ,text)))
      (overlay-put ov 'face 'combobulate-refactor-field-face)
      (overlay-put ov 'combobulate-refactor-field-transformer-fn transformer-fn)
      (combobulate--refactor-set-field ov text (symbol-name tag))
      ov)))

(defun combobulate--refactor-mark-cursor (pt)
  (let ((ov (make-overlay pt (1+ pt) nil t nil)))
    ;; args 2 and 3 refer to respectively `tag' and the default value.
    (overlay-put ov 'combobulate-refactor-actions `((cursor)))
    (overlay-put ov 'face 'combobulate-refactor-cursor-face)
    ov))

(defun combobulate--refactor-set-field (ov text &optional default-text)
  (let ((start (overlay-start ov))
        (s (if (length= text 0) default-text text))
        (transformer-fn (or (overlay-get ov 'combobulate-refactor-field-transformer-fn)
                            #'identity)))
    (delete-region start (overlay-end ov))
    (goto-char start)
    (insert (funcall transformer-fn s))
    (move-overlay ov start (point))))

(defun combobulate--refactor-field-has-tag-p (ov tag &optional action-name)
  (let ((actions (overlay-get ov 'combobulate-refactor-actions))
        (match))
    (pcase-dolist (`(,action ,ov-tag _) actions)
      (when (and (eq action (or action-name 'field)) (eq ov-tag tag))
        (setq match t)))
    match))

(defun combobulate--refactor-update-field (ov tag text &optional default-text)
  (let ((actions (overlay-get ov 'combobulate-refactor-actions)))
    (when (combobulate--refactor-field-has-tag-p ov tag)
      (map-put! actions 'field (cons tag text))
      (overlay-put ov 'combobulate-refactor-actions actions)
      (combobulate--refactor-set-field ov text default-text))
    ov))

(defun combobulate--refactor-mark-generic (beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'combobulate-refactor-actions '((generic)))
    (when combobulate-debug
      (overlay-put ov 'face 'diff-changed))
    ov))

(defun combobulate-reinitialize-parser ()
  "Force tree-sitter to reparse the buffer."
  (interactive)
  (dolist (parser (combobulate-parser-list))
    (combobulate-parser-create (combobulate-parser-language parser) nil t)
    (combobulate-parser-delete parser))
  (font-lock-flush))

(defun combobulate-drag-up (&optional arg)
  (interactive "^p")
  (with-argument-repetition arg
    (with-navigation-nodes (:skip-prefix t :procedures combobulate-navigation-sibling-procedures)
      (combobulate-visual-move-to-node (combobulate--drag 'up)))))

(defun combobulate-drag-down (&optional arg)
  (interactive "^p")
  (with-argument-repetition arg
    (with-navigation-nodes (:skip-prefix t :procedures combobulate-navigation-sibling-procedures)
      (combobulate-visual-move-to-node (combobulate--drag 'down)))))

(provide 'combobulate-manipulation)
;;; combobulate-manipulation.el ends here


