;; helper.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; taken from S. tanimoto, ch. #2&#3, ;;
;; on LISP, mathing, and pred calculus;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun match (p s)
 (cond
  ((handle-both-null p s))
  ((handle-normal-recursion p s))
  ((atom (first p)) nil)
  ((handle-? p s))
  ((handle-* p s))
  ((handle-restrict-pred p s))
  (t nil)
))

(defun handle-both-null (p s)
  (if (and (null p)(null s)) '((yes . yes)) ) )

(defun handle-normal-recursion (p s)
 (if (atom (first p)) (if (eql (first p)(first s)) (match (rest p)(rest s)) ) ) )

(defun handle-? (p s)
 (if s
  (if (eql (first (first p)) '?)
   (let ((rest-match (match (rest p)(rest s))))
   (if rest-match
    (acons (first (rest (first p)))
    (first s)
    rest-match) )))) )

(defun handle-* (p s)
 (if (eql (first (first p)) '*)
 (let ((pattern-variable (first (rest (first p)))) rest-match)
 (cond ; subcase 1 --match one element of S:
  ((and s
  (setf rest-match
   (match (rest p)(rest s)) ) )
   (acons pattern-variable
   (list (first s)) rest-match))
  ((setf rest-match (match (rest p) s))
   (acons pattern-variable nil rest-match) )
  ((and s (setf rest-match (match p (rest s)) ) )
   (acons pattern-variable
   (cons (first s)
   (val pattern-variable rest-match) ) (rest rest-match)) )
  (t nil) ))) )

(defun handle-restrict-pred (p s)
 (if s
 (if (member (first (first p)) '(? *))
  nil
 (if (apply (caar p) (list (first s)))
  (let ((rest-match (match (rest p) (rest s)))
  (pattern-variable (first (rest (first p)))) )
  (if rest-match
   (acons pattern-variable
   (first s)
 rest-match) )))) ) )

(defun val (variable alist) (rest (assoc variable alist)) )
