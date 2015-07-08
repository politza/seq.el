;;; seq.el --- Sequence manipulation functions  -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015 Free Software Foundation, Inc.

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: sequences
;; Version: 1.7
;; Package: seq

;; Maintainer: emacs-devel@gnu.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Sequence-manipulation functions that complement basic functions
;; provided by subr.el.
;;
;; All functions are prefixed with "seq-".
;;
;; All provided functions work on lists, strings and vectors.
;;
;; Functions taking a predicate or iterating over a sequence using a
;; function as argument take the function as their first argument and
;; the sequence as their second argument.  All other functions take
;; the sequence as their first argument.
;;
;; All functions are tested in test/automated/seq-tests.el

;;; Code:

(eval-when-compile (require 'cl-lib))

(defmacro seq-doseq (spec &rest body)
  "Loop over a sequence.
Similar to `dolist' but can be applied to lists, strings, and vectors.

Evaluate BODY with VAR bound to each element of SEQ, in turn.

\(fn (VAR SEQ) BODY...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  (let ((length (make-symbol "length"))
        (seq (make-symbol "seq"))
        (index (make-symbol "index")))
    `(let* ((,seq ,(cadr spec))
            (,length (if (listp ,seq) nil (seq-length ,seq)))
            (,index (if ,length 0 ,seq)))
       (while (if ,length
                  (< ,index ,length)
                (consp ,index))
         (let ((,(car spec) (if ,length
                                (prog1 (seq-elt ,seq ,index)
                                  (setq ,index (+ ,index 1)))
                              (pop ,index))))
           ,@body)))))

(if (fboundp 'pcase-defmacro)
    ;; Implementation of `seq-let' based on a `pcase'
    ;; pattern. Requires Emacs>=25.1.
    (progn
      (pcase-defmacro seq (&rest args)
        "pcase pattern matching sequence elements.
Matches if the object is a sequence (list, string or vector), and
binds each element of ARGS to the corresponding element of the
sequence."
        `(and (pred seq-p)
              ,@(seq--make-pcase-bindings args)))

      (defmacro seq-let (args seq &rest body)
        "Bind the variables in ARGS to the elements of SEQ then evaluate BODY.

ARGS can also include the `&rest' marker followed by a variable
name to be bound to the rest of SEQ."
        (declare (indent 2) (debug t))
        `(pcase-let ((,(seq--make-pcase-patterns args) ,seq))
           ,@body)))

  ;; Implementation of `seq-let' compatible with Emacs<25.1.
  (defmacro seq-let (args seq &rest body)
    "Bind the variables in ARGS to the elements of SEQ then evaluate BODY.

ARGS can also include the `&rest' marker followed by a variable
name to be bound to the rest of SEQ."
    (declare (indent 2) (debug t))
    (let ((seq-var (make-symbol "seq")))
      `(let* ((,seq-var ,seq)
              ,@(seq--make-bindings args seq-var))
         ,@body))))

(defun seq-drop (seq n)
  "Return a subsequence of SEQ without its first N elements.
The result is a sequence of the same type as SEQ.

If N is a negative integer or zero, SEQ is returned."
  (if (<= n 0)
      seq
    (if (listp seq)
        (seq--drop-list seq n)
      (let ((length (seq-length seq)))
        (seq-subseq seq (min n length) length)))))

(defun seq-take (seq n)
  "Return a subsequence of SEQ with its first N elements.
The result is a sequence of the same type as SEQ.

If N is a negative integer or zero, an empty sequence is
returned."
  (if (listp seq)
      (seq--take-list seq n)
    (seq-subseq seq 0 (min (max n 0) (seq-length seq)))))

(defun seq-drop-while (pred seq)
  "Return a sequence from the first element for which (PRED element) is nil in SEQ.
The result is a sequence of the same type as SEQ."
  (if (listp seq)
      (seq--drop-while-list pred seq)
    (seq-drop seq (seq--count-successive pred seq))))

(defun seq-take-while (pred seq)
  "Return the successive elements for which (PRED element) is non-nil in SEQ.
The result is a sequence of the same type as SEQ."
  (if (listp seq)
      (seq--take-while-list pred seq)
    (seq-take seq (seq--count-successive pred seq))))

(defun seq-filter (pred seq)
  "Return a list of all the elements for which (PRED element) is non-nil in SEQ."
  (let ((exclude (make-symbol "exclude")))
    (delq exclude (seq-map (lambda (elt)
                             (if (funcall pred elt)
                                 elt
                               exclude))
                           seq))))

(defun seq-remove (pred seq)
  "Return a list of all the elements for which (PRED element) is nil in SEQ."
  (seq-filter (lambda (elt) (not (funcall pred elt)))
              seq))

(defun seq-reduce (function seq initial-value)
  "Reduce the function FUNCTION across SEQ, starting with INITIAL-VALUE.

Return the result of calling FUNCTION with INITIAL-VALUE and the
first element of SEQ, then calling FUNCTION with that result and
the second element of SEQ, then with that result and the third
element of SEQ, etc.

If SEQ is empty, return INITIAL-VALUE and FUNCTION is not called."
  (if (seq-empty-p seq)
      initial-value
    (let ((acc initial-value))
      (seq-doseq (elt seq)
        (setq acc (funcall function acc elt)))
      acc)))

(defun seq-some-p (pred seq)
  "Return any element for which (PRED element) is non-nil in SEQ, nil otherwise."
  (catch 'seq--break
    (seq-doseq (elt seq)
      (when (funcall pred elt)
        (throw 'seq--break elt)))
    nil))

(defun seq-every-p (pred seq)
  "Return non-nil if (PRED element) is non-nil for all elements of the sequence SEQ."
  (catch 'seq--break
    (seq-doseq (elt seq)
      (or (funcall pred elt)
          (throw 'seq--break nil)))
    t))

(defun seq-count (pred seq)
  "Return the number of elements for which (PRED element) is non-nil in SEQ."
  (let ((count 0))
    (seq-doseq (elt seq)
      (when (funcall pred elt)
        (setq count (+ 1 count))))
    count))

(defun seq-empty-p (seq)
  "Return non-nil if the sequence SEQ is empty, nil otherwise."
  (if (listp seq)
      (null seq)
    (= 0 (seq-length seq))))

(defun seq-sort (pred seq)
  "Return a sorted sequence comparing using PRED the elements of SEQ.
The result is a sequence of the same type as SEQ."
  (if (listp seq)
      (sort (seq-copy seq) pred)
    (let ((result (seq-sort pred (append seq nil))))
      (seq-into result (type-of seq)))))

(defun seq-contains-p (seq elt &optional testfn)
  "Return the first element in SEQ that equals to ELT.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (seq-some-p (lambda (e)
                (funcall (or testfn #'equal) elt e))
              seq))

(defun seq-uniq (seq &optional testfn)
  "Return a list of the elements of SEQ with duplicates removed.
TESTFN is used to compare elements, or `equal' if TESTFN is nil."
  (let ((result '()))
    (seq-doseq (elt seq)
      (unless (seq-contains-p result elt testfn)
        (setq result (cons elt result))))
    (nreverse result)))

(defun seq-subseq (seq start &optional end)
  "Return the subsequence of SEQ from START to END.
If END is omitted, it defaults to the length of the sequence.
If START or END is negative, it counts from the end."
  (cond ((or (stringp seq) (vectorp seq)) (substring seq start end))
        ((listp seq)
         (let (len (errtext (format "Bad bounding indices: %s, %s" start end)))
           (and end (< end 0) (setq end (+ end (setq len (seq-length seq)))))
           (if (< start 0) (setq start (+ start (or len (setq len (seq-length seq))))))
           (when (> start 0)
             (setq seq (nthcdr (1- start) seq))
             (or seq (error "%s" errtext))
             (setq seq (cdr seq)))
           (if end
               (let ((res nil))
                 (while (and (>= (setq end (1- end)) start) seq)
                   (push (pop seq) res))
                 (or (= (1+ end) start) (error "%s" errtext))
                 (nreverse res))
             (seq-copy seq))))
        (t (error "Unsupported sequence: %s" seq))))

(defun seq-concatenate (type &rest seqs)
  "Concatenate, into a sequence of type TYPE, the sequences SEQS.
TYPE must be one of following symbols: vector, string or list.

\n(fn TYPE SEQUENCE...)"
  (pcase type
    (`vector (apply #'vconcat seqs))
    (`string (apply #'concat seqs))
    (`list (apply #'append (append seqs '(nil))))
    (t (error "Not a sequence type name: %S" type))))

(defun seq-mapcat (function seq &optional type)
  "Concatenate the result of applying FUNCTION to each element of SEQ.
The result is a sequence of type TYPE, or a list if TYPE is nil."
  (apply #'seq-concatenate (or type 'list)
         (seq-map function seq)))

(defun seq-partition (seq n)
  "Return a list of the elements of SEQ grouped into sub-sequences of length N.
The last sequence may contain less than N elements.  If N is a
negative integer or 0, nil is returned."
  (unless (< n 1)
    (let ((result '()))
      (while (not (seq-empty-p seq))
        (push (seq-take seq n) result)
        (setq seq (seq-drop seq n)))
      (nreverse result))))

(defun seq-intersection (seq1 seq2 &optional testfn)
  "Return a list of the elements that appear in both SEQ1 and SEQ2.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (seq-reduce (lambda (acc elt)
                (if (seq-contains-p seq2 elt testfn)
                    (cons elt acc)
                  acc))
              (seq-reverse seq1)
              '()))

(defun seq-difference (seq1 seq2 &optional testfn)
  "Return a list of th elements that appear in SEQ1 but not in SEQ2.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (seq-reduce (lambda (acc elt)
                (if (not (seq-contains-p seq2 elt testfn))
                    (cons elt acc)
                  acc))
              (seq-reverse seq1)
              '()))

(defun seq-group-by (function seq)
  "Apply FUNCTION to each element of SEQ.
Separate the elements of SEQ into an alist using the results as
keys.  Keys are compared using `equal'."
  (seq-reduce
   (lambda (acc elt)
     (let* ((key (funcall function elt))
            (cell (assoc key acc)))
       (if cell
           (setcdr cell (push elt (cdr cell)))
         (push (list key elt) acc))
       acc))
   (seq-reverse seq)
   nil))

(defalias 'seq-reverse
  (if (ignore-errors (reverse [1 2]))
      #'reverse
    (lambda (seq)
      "Return the reversed copy of list, vector, or string SEQ.
See also the function `nreverse', which is used more often."
      (let ((result '()))
        (seq-map (lambda (elt) (push elt result))
                 seq)
        (if (listp seq)
            result
          (seq-into result (type-of seq)))))))

(defun seq-into (seq type)
  "Convert the sequence SEQ into a sequence of type TYPE.
TYPE can be one of the following symbols: vector, string or list."
  (pcase type
    (`vector (vconcat seq))
    (`string (concat seq))
    (`list (append seq nil))
    (t (error "Not a sequence type name: %S" type))))

(defmacro seq--with-matrix-macros (&rest body)
  (declare (indent 0) (debug t))
  `(cl-macrolet ((make-matrix (rows columns &optional init-value)
                   (list 'apply (list 'quote 'vector)
                         (list 'cl-loop 'for 'i 'from 1 'to rows
                               'collect (list 'make-vector columns init-value))))
                 (mset (matrix row column newelt)
                   (list 'aset (list 'aref matrix row) column newelt))
                 (mref (matrix row column)
                   (list 'aref (list 'aref matrix row) column)))
     ,@body))

(defun seq-alignment (seq1 seq2 &optional
                           similarity-fn
                           gap-penalty
                           _alignment-type
                           score-only-p
                           gap-symbol)
  "Return an alignment of sequences SEQ1 and SEQ2.

SIMILARITY-FN should be a function. It is called with two
arguments: One element from SEQ1 and one from SEQ2 and it should
return a number determining how similar the elements are, where
higher values mean `more similar'.  The default returns 1 if the
elements are equal, else -1.

GAP-PENALTY is the penalty for one single gap in the alignment,
the default is -1.

ALIGNMENT-TYPE may be one of the symbols `prefix', `suffix',
`infix' or nil.  If it is `prefix' \(resp. `suffix'\), trailing
\(resp. preceding\) elements in SEQ2 may be ignored, i.e. deleted
without incurring a penalty; `infix' is the combination of both.

Return a cons \(SCORE . ALINGMENT\), unless SCORE-ONLY-P is
non-nil, in which case only SCORE is returned.  SCORE says how
similar the sequences are and ALINGMENT is a list of \(E1 . E2\),
where E1 is an element from SEQ1 or nil, likewise for E2.  If one
of them is nil, it means there is a gap at this position in the
respective sequence in the alignment."

  ;; See https://en.wikipedia.org/wiki/Needleman-Wunsch_algorithm
  (seq--with-matrix-macros
    (let* ((len1 (length seq1))
           (len2 (length seq2))
           (d (make-matrix (1+ len1) (1+ len2))))
      
      (unless similarity-fn
        (setq similarity-fn
              (lambda (a b)
                (if (equal a b) 1 -1))))
      (unless gap-penalty
        (setq gap-penalty -1))

      (cl-loop for i from 0 to len1 do
        (mset d i 0 (* i gap-penalty)))
      (cl-loop for j from 0 to len2 do
        (mset d 0 j (* j gap-penalty)))

      (cl-loop for i from 1 to len1 do
        (cl-loop for j from 1 to len2 do
          (let ((max (max
                      (+ (mref d (1- i) j) gap-penalty)
                      (+ (mref d i (1- j))
                         gap-penalty)
                      (+ (mref d (1- i) (1- j))
                         (funcall similarity-fn
                                  (elt seq1 (1- i))
                                  (elt seq2 (1- j)))))))
            (mset d i j max))))

      (if score-only-p
          (mref d len1 len2)
        (let ((i len1)
              (j len2)
              alignment)
          (while (or (> i 0)
                     (> j 0))
            (cond
             ((and (> i 0)
                   (= (mref d i j)
                      (+ (mref d (1- i) j)
                         gap-penalty)))
              (cl-assert (> i 0) t)
              (cl-decf i)
              (push (cons (elt seq1 i) gap-symbol) alignment))
             ((and (> j 0)
                   (= (mref d i j)
                      (+ (mref d i (1- j))
                         gap-penalty)))
              (cl-assert (> j 0) t)
              (cl-decf j)
              (push (cons gap-symbol (elt seq2 j)) alignment))
             (t
              (cl-assert (and (> i 0) (> j 0)) t)
              (cl-decf i)
              (cl-decf j)
              (push (cons (elt seq1 i)
                          (elt seq2 j)) alignment))))
          (cons (mref d len1 len2) alignment))))))

(defun seq-ralignment (seq1 seq2 &optional
                            similarity-fn
                            gap-penalty
                            _alignment-type
                            score-only-p
                            gap-symbol)
  ;; See https://en.wikipedia.org/wiki/Needleman-Wunsch_algorithm
  (seq--with-matrix-macros
    (let* ((len1 (length seq1))
           (len2 (length seq2))
           (d (make-matrix (1+ len1) (1+ len2))))
      
      (unless similarity-fn
        (setq similarity-fn
              (lambda (a b)
                (if (equal a b) 1 -1000))))
      (unless gap-penalty
        (setq gap-penalty -1))

      (cl-loop for i from 0 to len1 do
        (mset d i 0 (* i gap-penalty)))
      (cl-loop for j from 0 to len2 do
        (mset d 0 j (* j gap-penalty)))

      (cl-loop for i from 1 to len1 do
        (cl-loop for j from 1 to len2 do
          (let ((max (max
                      (+ (mref d (1- i) j) gap-penalty)
                      (+ (mref d i (1- j))
                         gap-penalty)
                      (+ (mref d (1- i) (1- j))
                         (let ((s1 (elt seq1 (1- i)))
                               (s2 (elt seq2 (1- j))))
                           (if (and (consp s1)
                                    (consp s2))
                               (seq-ralignment
                                s1 s2 similarity-fn gap-penalty nil t gap-symbol)
                             (funcall similarity-fn s1 s2)))))))
            (mset d i j max))))

      (if score-only-p
          (mref d len1 len2)
        (let ((i len1)
              (j len2)
              alignment)
          (while (or (> i 0)
                     (> j 0))
            (cond
             ((and (> i 0)
                   (= (mref d i j)
                      (+ (mref d (1- i) j)
                         gap-penalty)))
              (cl-assert (> i 0) t)
              (cl-decf i)
              (push (cons (elt seq1 i) gap-symbol) alignment))
             ((and (> j 0)
                   (= (mref d i j)
                      (+ (mref d i (1- j))
                         gap-penalty)))
              (cl-assert (> j 0) t)
              (cl-decf j)
              (push (cons gap-symbol (elt seq2 j)) alignment))
             (t
              (cl-assert (and (> i 0) (> j 0)) t)
              (cl-decf i)
              (cl-decf j)
              (let ((s1 (elt seq1 i))
                    (s2 (elt seq2 j)))
                (push
                 (if (and (consp s1)
                          (consp s2))
                     (cons :rec
                       (cdr
                        (seq-ralignment
                         s1 s2 similarity-fn gap-penalty nil nil gap-symbol)))
                   (cons s1 s2))
                 alignment)))))
          (cons (mref d len1 len2) alignment))))))

(defun seq-merge (seq1 seq2 &optional
                       similarity-fn
                       gap-penalty
                       score-only-p)
  ;; See https://en.wikipedia.org/wiki/Needleman-Wunsch_algorithm
  (seq--with-matrix-macros
    (let* ((len1 (length seq1))
           (len2 (length seq2))
           (d (make-matrix (1+ len1) (1+ len2))))
      
      (unless similarity-fn
        (setq similarity-fn
              (lambda (a b)
                (if (equal a b) 1 0))))
      (unless gap-penalty
        (setq gap-penalty 0))

      (cl-loop for i from 0 to len1 do
        (mset d i 0 (* i gap-penalty)))
      (cl-loop for j from 0 to len2 do
        (mset d 0 j (* j gap-penalty)))

      (cl-loop for i from 1 to len1 do
        (cl-loop for j from 1 to len2 do
          (let ((max (max
                      (+ (mref d (1- i) j) gap-penalty)
                      (+ (mref d i (1- j))
                         gap-penalty)
                      (+ (mref d (1- i) (1- j))
                         (let ((s1 (elt seq1 (1- i)))
                               (s2 (elt seq2 (1- j))))
                           (if (and (consp s1)
                                    (consp s2))
                               (seq-merge
                                s1 s2 similarity-fn gap-penalty t)
                             (funcall similarity-fn s1 s2)))))))
            (mset d i j max))))

      (if score-only-p
          (mref d len1 len2)
        (let ((i len1)
              (j len2)
              (gap-symbol '$)
              merged)
          (while (or (> i 0)
                     (> j 0))
            (cond
             ((and (> i 0)
                   (= (mref d i j)
                      (+ (mref d (1- i) j)
                         gap-penalty)))
              (cl-assert (> i 0) t)
              (cl-decf i)
              (unless (eq (car merged)
                          gap-symbol)
                (push gap-symbol merged)))
             ((and (> j 0)
                   (= (mref d i j)
                      (+ (mref d i (1- j))
                         gap-penalty)))
              (cl-assert (> j 0) t)
              (cl-decf j)
              (unless (eq (car merged)
                          gap-symbol)
                (push gap-symbol merged)))
             (t
              (cl-assert (and (> i 0) (> j 0)) t)
              (cl-decf i)
              (cl-decf j)
              (let ((s1 (elt seq1 i))
                    (s2 (elt seq2 j)))
                (cl-assert (or (and (consp s1)
                                    (consp s2))
                               (equal s1 s2)))
                (cond
                 ((and (consp s1)
                       (consp s2))
                  (push
                   (cdr
                    (seq-merge
                     s1 s2 similarity-fn gap-penalty))
                   merged))
                 ((equal s1 s2)
                  (push s1 merged))
                 ((not (eq (car merged) gap-symbol))
                  (push gap-symbol merged)))))))
          (cons (mref d len1 len2) merged))))))

(defun bm ()
  (interactive)
  (progn
    (garbage-collect)
    (benchmark
     1000
     '(dolist (s1 sequences)
        (dolist (s2 sequences)
          (seq-alignment s1 s2 nil nil nil nil))))))

(defun sa-rec-fn (elt1 elt2)
  (cond
   ((and (consp elt1)
         (consp elt2))
    (seq-alignment elt1 elt2 'sa-rec-fn nil nil nil))
   ((equal elt1 elt2) 1)
   (t -1)))

(defun sa-rec ()
  (let ((s1 '(list (symbol "defun")
                   (symbol "seq" "-" "alignment")
                   (list (symbol "seq1" "seq2" "flag"))))
        (s2
         '(list (symbol "defun")
                (symbol "seq" "-" "something")
                (list (symbol "seq1" "seq2")))))
    (seq-merge s1 s2)))
         
(defun seq-upgma-test ()
  (seq--with-matrix-macros
   (let ((d [[nil nil nil nil nil nil nil]
             [19.0 nil nil nil nil nil nil]
             [27.0 31.0 nil nil nil nil nil]
             [8.0 18.0 26.0 nil nil nil nil]
             [33.0 36.0 41.0 31.0 nil nil nil]
             [18.0 1.0 32.0 17.0 35.0 nil nil]
             [13.0 13.0 29.0 14.0 28.0 12.0 nil]]))
     (seq-upgma [A B C D E F G]
                (lambda (i j)
                  (mref d j i))))))

(defun seq-upgma (seq distance-fn)
  (seq--with-matrix-macros
    (let* ((len (length seq))
           (dist (make-matrix len len 0.0))
           (m (make-matrix len len 0.0))
           (indices (number-sequence 0 (1- len)))
           (tree (apply 'vector indices))
           (joined (apply 'vector (mapcar 'list indices))))
      (cl-loop for i from 0 below len do
        (cl-loop for j from (1+ i) below len do
          (mset m i j (funcall distance-fn i j))
          (mset dist i j (mref m i j))))
      (cl-labels
        (;; (print ()
         ;;   (with-output-to-string
         ;;     (cl-loop for li on indices do 
         ;;       (let ((i (car li)))
         ;;         (cl-loop for lj on (cdr li) do
         ;;           (let ((j (car lj)))
         ;;             (princ (format "%5.2f " (mref m i j)))))
         ;;         (terpri)))))
         (dmin ()
           (let (min min-nth)
             (cl-loop for li on indices do
               (let ((i (car li)))
                 (cl-loop for lj on (cdr li) do
                   (let* ((j (car lj))
                          (d (mref m i j)))
                     (when (or (null min)
                               (< d min))
                       (setq min d
                             min-nth (list i j)))))))
             min-nth))
         (join (i j)
           (aset tree i (cons (aref tree i)
                              (aref tree j)))
           (aset tree j nil)
           (aset joined i (append (aref joined i)
                                  (aref joined j)))
           (setq indices (delq j indices))
           (cl-loop for k in indices do
             (when (/= k i)
               (let ((s 0.0)
                     (l 0))
                 (cl-loop for x in (aref joined i) do
                   (cl-loop for y in (aref joined k) do
                     (cl-incf s (mref dist (min x y) (max x y)))
                     (cl-incf l)))
                 (mset m (min k i) (max k i) (/ s l)))))))
        (dotimes (_ (1- len))
          (apply #'join (dmin)))
        (car (remq nil (append tree nil)))))))

(comment
  (seq-multi-alignment
   '((a b c)
     (d e f g)
     (a d b c))))

(defun seq-multi-alignment--effective-scores (seqs &optional similarity-fn gap-penalty)
  (seq--with-matrix-macros
    (let* ((len (length seqs))
           (scores (make-matrix len len nil))
           (auto-scores (make-vector len nil))
           (i 0))
      (cl-loop for s in seqs do
        (aset auto-scores i (seq-alignment
                             s s similarity-fn gap-penalty nil t))
        (cl-incf i))
      (setq i 0)
      (cl-loop for rest on seqs do
        (let ((s1 (car rest))
              (j (1+ i)))
          (cl-loop for s2 in (cdr rest) do
            (let ((s (seq-alignment
                      s1 s2 similarity-fn gap-penalty nil t))
                  (smax (/ (+ (aref auto-scores i)
                              (aref auto-scores j))
                           2.0))
                  (srand (seq-alignment (seq-shuffle s1)
                                        (seq-shuffle s2)
                                        similarity-fn
                                        gap-penalty nil t)))
              (mset scores i j
                    (- (log (max 0 (/ (float (- s srand)) (- smax srand)))))))
            (cl-incf j))
          (cl-incf i)))
      scores)))
  
(defun seq-multi-alignment--guide-tree (seqs &optional similarity-fn gap-penalty)
  (let ((scores (seq-multi-alignment--effective-scores
                 seqs similarity-fn gap-penalty)))
    (seq-upgma seqs (lambda (i j)
                      (mref scores i j)))))
      
(defun seq-multi-alignment--align (tree sequences score-fn gap-penalty gap-symbol)
  (if (numberp tree)
      (list (elt sequences tree))
    (let* ((groups (list (seq-multi-alignment--align
                          (car tree) sequences score-fn
                          gap-penalty gap-symbol)
                         (seq-multi-alignment--align
                          (cdr tree) sequences score-fn
                          gap-penalty gap-symbol)))
           (group-ncolumns (list (length (car (elt groups 0)))
                                 (length (car (elt groups 1)))))
           (group-sizes (list (length (elt groups 0))
                              (length (elt groups 1))))
           (pair-score-fn (lambda (col1 col2)
                            (let ((score 0))
                              (seq-doseq (s1 (car groups))
                                (seq-doseq (s2 (cadr groups))
                                  (cl-incf score
                                           (funcall score-fn
                                                    (elt s1 col1)
                                                    (elt s2 col2)))))
                              score)))
           (alignment (cdr (seq-alignment
                            (number-sequence 0 (1- (elt group-ncolumns 0)))
                            (number-sequence 0 (1- (elt group-ncolumns 1)))
                            pair-score-fn gap-penalty)))
           (aligned (list (make-list (elt group-sizes 0) nil)
                          (make-list (elt group-sizes 1) nil))))
      (dolist (elt alignment)
        (dotimes (i 2)
          (if-let ((idx (if (= i 0) (car elt) (cdr elt))))
              (cl-loop for sg on (elt groups i)
                for cons on (elt aligned i) do
                (push (pop (car sg)) (car cons)))
            (cl-loop for cons on (elt aligned i) do
              (push gap-symbol (car cons))))))
      (append (mapcar 'reverse (elt aligned 0))
              (mapcar 'reverse (elt aligned 1))))))

(defun seq-multi-alignment (sequences &optional score-fn gap-penalty gap-symbol)
  (unless score-fn
    (setq score-fn (lambda (elt1 elt2) (if (equal elt1 elt2) 1 -1))))
  (unless gap-penalty
    (setq gap-penalty -1))
  (let* ((tree (seq-multi-alignment--guide-tree
                sequences score-fn gap-penalty))
         (hash (make-hash-table :test 'equal))
         (multi-score-fn
          (lambda (elt1 elt2)
            (cond
             ((and (eq elt1 gap-symbol)
                   (eq elt2 gap-symbol)) 0)
             ((or (eq elt1 gap-symbol)
                  (eq elt2 gap-symbol)) gap-penalty)
             (t
              (let* ((key (list elt1 elt2))
                     (score (gethash key hash)))
                (unless score
                  (setq score (funcall score-fn elt1 elt2))
                  (puthash key score hash)
                  (puthash (reverse key) score hash))
                score))))))
    (seq-multi-alignment--align
     tree sequences multi-score-fn gap-penalty gap-symbol)))
    

(defun seq-shuffle (seq)
  (let ((vec (if (vectorp seq)
                 (copy-sequence seq)
               (apply 'vector (append seq nil)))))
    (cl-loop for i from (1- (length vec)) downto 1 do
      (let* ((j (random i))
             (tmp (aref vec j)))
        (aset vec j (aref vec i))
        (aset vec i tmp)))
    vec))      

(defun seq-multi-alignment-display (sequences &optional score-fn gap-penalty gap-symbol)
  (interactive)
  (let* ((ses-initial-column-width 16)
         (aligned (seq-multi-alignment
                   sequences score-fn gap-penalty gap-symbol))
         (col 0)
         (widths (make-list (length (car aligned)) 0))
         (cells (mapconcat
                 (lambda (s)
                   (setq col 0)
                   (mapconcat (lambda (elt)
                                (let ((str (replace-regexp-in-string
                                            "[\t\n]"
                                            (lambda (m)
                                              (if (equal m "\n")
                                                  "\\n"
                                                "\\t"))
                                            (prin1-to-string elt)
                                            t t)))
                                  (setf (elt widths col)
                                        (max (elt widths col)
                                             (length str)))
                                  (cl-incf col)
                                  str))
                              s "\t"))
                 aligned "\n")))
    (with-current-buffer (get-buffer-create "*Alignment*")
      (let ((inhibit-read-only t))
        (erase-buffer))
      (ses-mode)
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) t)))
        (ses-yank-tsf cells nil))
      (dotimes (col (length widths))
        (ses-set-column-width col (+ (elt widths col))))
      (ses-command-hook)
      (pop-to-buffer (current-buffer)))))

(setq sequences
      (list
       (split-string "if [ $# -ne 1 ]; then echo \"usage:eunalias NAME\" return 1; fi" " +" t)
       (split-string "if [ $# -ne 1 ]; then echo \"usage:eunalias NAME\" return 1; fi" " +" t)
       (split-string "if [ $# -gt 1 ]; then echo \"usage:$FUNCNAME [name]\" >&2 return 1 fi" " +" t)
       (split-string "if [ $# -gt 1 ]; then echo \"usage:$FUNCNAME [name]\" >&2 return 1 fi" " +" t)
       (split-string "if [ -z \"$spec\" ]; then echo \"No such alias: $name\" >&2 return 1 fi" " +" t)
       (split-string "if [ -z \"$spec\" ]; then echo \"No such alias: $name\" >&2 return 1 fi" " +" t)
       (split-string "if [ -z \"$spec\" ]; then echo \"No such alias: $name\" >&2 return 1; fi" " +" t)
       (split-string "if [ -z \"$spec\" ]; then echo \"No such alias: $name\" >&2 return 1; fi" " +" t)
       (split-string "if [ \"$fn\" == \"$spec\" ]; then eargs= fi" " +" t)
       (split-string "if [ \"$fn\" == \"$spec\" ]; then eargs= fi" " +" t)))

(defun seq-edit-distance (seq1 seq2 &optional
                               max-distance
                               allow-transposition
                               score-only-p)
  "Compute the Levenshtein distance of sequence SEQ1 and SEQ2

MAX-DISTANCE is the maximal expected distance, i.e. if the real
distance is greater than this value, this function returns nil.
Lower values result in better performance. If MAX-DISTANCE is
nil, the proper distance is always returned.

If ALLOW-TRANSPOSITION is non-nil, also recognize the transposition
of elements as one atomic operation.

See `seq-alignment' for the return value of this function and the
SCORE-ONLY-P argument."

  ;; See `Algorithms for Approximate String Matching', E. Ukkonen

  (seq--with-matrix-macros
    (let* ((nil-value nil)
           (len1 (length seq1))
           (len2 (length seq2))
           (infinity (+ 1 len2 len1))
           (p1 1)
           (p2 (+ len1 len2))
           (k (or max-distance infinity)))

      (unless (> (abs (- len2 len1)) k)
        (let* ((d (make-matrix (1+ len1) (1+ len2) infinity))
               (p (ceiling (/ (- k (abs (- len2 len1))) 2.0)))
               (j 0)
               i i-end (ok t))

          (while (and ok (<= p1 p2) (<= j len2))
            (setq ok nil)
            (if (>= len1 len2)
                (setq i (max 0 (1- p1) (- j p))
                      i-end (min len1 (+ j (- len1 len2) p)))
              (setq i (max 0 (1- p1) (- (+ j (- len1 len2)) p))
                    i-end (min len1 (+ j p))))
            (while (<=  i i-end)
              (cond
               ((= i 0)
                (mset d i j j))
               ((= j 0)
                (mset d i j i))
               (t
                (mset d i j
                      (min
                       (1+ (mref d (1- i) j))
                       (1+ (mref d i (1- j)))
                       (+ (mref d (1- i) (1- j))
                          (if (equal
                               (elt seq1 (1- i))
                               (elt seq2 (1- j)))
                              0 1))
                       (or (and allow-transposition
                                (> i 1)
                                (> j 1)
                                (= (elt seq1 (- i 2))
                                   (elt seq2 (- j 1)))
                                (= (elt seq1 (- i 1))
                                   (elt seq2 (- j 2)))
                                (1+ (mref d (- i 2) (- j 2))))
                           infinity)))))
              (if (<= (mref d i j) k)
                  (if (not ok)
                      (setq ok t
                            p1 i)
                    (if (= i i-end)
                        (setq p2 i)))
                (if (not ok)
                    (setq p1 (1+ i))
                  (setq p2 (1- i))))
              (setq i (1+ i)))
            (setq j (1+ j)))

          (when (<= (mref d len1 len2) k)
            (if score-only-p
                (mref d len1 len2)
              (let ((i len1)
                    (j len2)
                    mapping)
                (while (or (> i 0)
                           (> j 0))
                  (cond
                   ((and (> i 0)
                         (= (mref d i j)
                            (1+ (mref d (1- i) j))))
                    (cl-decf i)
                    (push (cons (elt seq1 i) nil-value) mapping))
                   ((and (> j 0)
                         (= (mref d i j)
                            (1+ (mref d i (1- j)))))
                    (cl-decf j)
                    (push (cons nil-value (elt seq2 j)) mapping))
                   (t
                    (cl-assert (and (> i 0) (> j 0)) t)
                    (cl-decf i)
                    (cl-decf j)
                    (push (cons (elt seq1 i)
                                (elt seq2 j)) mapping))))
                (cons (mref d len1 len2) mapping)))))))))


(defun seq--drop-list (list n)
  "Return a list from LIST without its first N elements.
This is an optimization for lists in `seq-drop'."
  (while (and list (> n 0))
    (setq list (cdr list)
          n (1- n)))
  list)

(defun seq--take-list (list n)
  "Return a list from LIST made of its first N elements.
This is an optimization for lists in `seq-take'."
  (let ((result '()))
    (while (and list (> n 0))
      (setq n (1- n))
      (push (pop list) result))
    (nreverse result)))

(defun seq--drop-while-list (pred list)
  "Return a list from the first element for which (PRED element) is nil in LIST.
This is an optimization for lists in `seq-drop-while'."
  (while (and list (funcall pred (car list)))
    (setq list (cdr list)))
  list)

(defun seq--take-while-list (pred list)
  "Return the successive elements for which (PRED element) is non-nil in LIST.
This is an optimization for lists in `seq-take-while'."
  (let ((result '()))
    (while (and list (funcall pred (car list)))
      (push (pop list) result))
    (nreverse result)))

(defun seq--count-successive (pred seq)
  "Return the number of successive elements for which (PRED element) is non-nil in SEQ."
  (let ((n 0)
        (len (seq-length seq)))
    (while (and (< n len)
                (funcall pred (seq-elt seq n)))
      (setq n (+ 1 n)))
    n))

(defun seq--make-pcase-bindings (args)
  "Return a list of bindings of the variables in ARGS to the elements of a sequence."
  (let ((bindings '())
        (index 0)
        (rest-marker nil))
    (seq-doseq (name args)
      (unless rest-marker
        (pcase name
          (`&rest
           (progn (push `(app (pcase--flip seq-drop ,index)
                              ,(seq--elt-safe args (1+ index)))
                        bindings)
                  (setq rest-marker t)))
          (t
           (push `(app (pcase--flip seq--elt-safe ,index) ,name) bindings))))
      (setq index (1+ index)))
    bindings))

(defun seq--make-pcase-patterns (args)
  "Return a list of `(seq ...)' pcase patterns from the argument list ARGS."
  (cons 'seq
        (seq-map (lambda (elt)
                   (if (seq-p elt)
                       (seq--make-pcase-patterns elt)
                     elt))
                 args)))

;; Helper function for the Backward-compatible version of `seq-let'
;; for Emacs<25.1.
(defun seq--make-bindings (args seq &optional bindings)
  "Return a list of bindings of the variables in ARGS to the elements of a sequence.
if BINDINGS is non-nil, append new bindings to it, and return
BINDINGS."
  (let ((index 0)
        (rest-marker nil))
    (seq-doseq (name args)
      (unless rest-marker
        (pcase name
          ((pred seq-p)
           (setq bindings (seq--make-bindings (seq--elt-safe args index)
                                              `(seq--elt-safe ,seq ,index)
                                              bindings)))
          (`&rest
           (progn (push `(,(seq--elt-safe args (1+ index))
                          (seq-drop ,seq ,index))
                        bindings)
                  (setq rest-marker t)))
          (t
           (push `(,name (seq--elt-safe ,seq ,index)) bindings))))
      (setq index (1+ index)))
    bindings))

(defun seq--elt-safe (seq n)
  "Return element of SEQ at the index N.
If no element is found, return nil."
  (when (or (listp seq)
            (and (sequencep seq)
                 (> (seq-length seq) n)))
    (seq-elt seq n)))

(defun seq--activate-font-lock-keywords ()
  "Activate font-lock keywords for some symbols defined in seq."
  (font-lock-add-keywords 'emacs-lisp-mode
                          '("\\<seq-doseq\\>" "\\<seq-let\\>")))

(defalias 'seq-copy #'copy-sequence)
(defalias 'seq-elt #'elt)
(defalias 'seq-length #'length)
(defalias 'seq-do #'mapc)
(defalias 'seq-each #'seq-do)
(defalias 'seq-map #'mapcar)
(defalias 'seq-p #'sequencep)

(unless (fboundp 'elisp--font-lock-flush-elisp-buffers)
  ;; In Emacs≥25, (via elisp--font-lock-flush-elisp-buffers and a few others)
  ;; we automatically highlight macros.
  (add-to-list 'emacs-lisp-mode-hook #'seq--activate-font-lock-keywords))

(provide 'seq)
;;; seq.el ends here
