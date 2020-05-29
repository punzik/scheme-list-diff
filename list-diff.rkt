#lang racket

(provide list-diff)

;;; Cyclic shift left
(define (shift-left lst)
  (reverse (cons (car lst) (reverse (cdr lst)))))

;;; Find longest overlap of two lists
;;; Returns (values <begin of overlap> <length of overlap>)
(define (max-overlap la lb same? [beg #f] [len #f] [n 0])
  (if (or (null? la) (null? lb)) (values beg len)
      (let-values (((pfx rla rlb) (split-common-prefix la lb (lambda (x y) (and x y (same? x y))))))
        (let ((l (length pfx)))
          (if (and (not (null? pfx)) (or (not len) (> l len)))
              (max-overlap rla rlb same? n l (+ n l))
              (max-overlap (cdr la) (cdr lb) same? beg len (+ n 1)))))))

;;; Shift lists relative to each other and find maximum correlation (overlap).
;;; Returns (values <a> <shifted b> <begin of overlap> <length of overlap>)
(define (correlation a b same? [n (length a)])
  (let-values (((ob ol) (max-overlap a b same?)))
    (if (zero? n)
        (values a b ob ol)
        (let-values (((an bn obn oln)
                      (correlation a (shift-left b) same? (- n 1))))
          (if (or (not ol)
                  (and oln (> oln ol)))
              (values an bn obn oln)
              (values a b ob ol))))))

;;; Split list to 3 pieces
(define (split3 lst beg len)
  (values (take lst beg)
          (take (drop lst beg) len)
          (drop (drop lst beg) len)))

;;; Make difference list
;;; Returns list of pair where car is item of
;;; list a or #f, cdr is item of list b of #f.
(define (make-diff-list a b)
  (foldr (lambda (ia ib out)
           (cond
            ((and (not ia) (not ib)) out)
            ((not ia) (cons (cons #f ib) out))
            ((not ib) (cons (cons ia #f) out))
            (else (cons (cons ia ib) out))))
         '() a b))

;;; Calculate lists difference
;;; All #f elements will be removed
(define (list-diff list-a list-b [same? equal?])
  ;; Remove false items
  (let ((list-a (filter values list-a))
        (list-b (filter values list-b)))
    ;; If empty lists then return '()
    (if (and (null? list-a) (null? list-b)) '()
        ;; Prepare lists for compare and shift
        (let* ((la (length list-a))
               (lb (length list-b))
               (a (append (make-list lb #f) list-a (make-list lb #f)))
               (b (append list-b (make-list la #f) (make-list lb #f))))
          ;; Find longest continous overlap of lists
          (let-values (((a b ob ol) (correlation a b same?)))
            (if ob
                ;; If found overlap split lists to 3 pieces -
                ;; <before overlap> <overlap> <after overlap>
                ;; and repeat diff for <before> and <after>
                (let-values (((a1 a2 a3) (split3 a ob ol))
                             ((b1 b2 b3) (split3 b ob ol)))
                  (append (list-diff a1 b1 same?)
                          (make-diff-list a2 b2)
                          (list-diff a3 b3 same?)))
                ;; If no overlap simple mark difference
                (make-diff-list
                 (append list-a (make-list lb #f))
                 (append list-b (make-list la #f)))))))))
