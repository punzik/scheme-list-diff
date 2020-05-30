#lang racket

(provide list-diff)

;;; Cyclic shift left
(define (shift-left lst)
  (reverse (cons (car lst) (reverse (cdr lst)))))

;;; Split list to 3 pieces
(define (split3 lst beg len)
  (values (take lst beg)
          (take (drop lst beg) len)
          (drop (drop lst beg) len)))

;;; No item flag and predicate
(define none (gensym "none"))
(define (none? x) (eq? x none))

;;; Calculate lists difference
;;; All #f elements will be removed
(define (list-diff list-a list-b [same? equal?])
  ;; Find longest overlap of two lists
  ;; Returns (values <begin of overlap> <length of overlap>)
  (define (max-overlap la lb [beg #f] [len #f] [n 0])
    (if (or (null? la) (null? lb)) (values beg len)
        (let-values (((pfx rla rlb) (split-common-prefix
                                     la lb
                                     (lambda (x y) (and
                                               (not (none? x))
                                               (not (none? y))
                                               (same? x y))))))
          (let ((l (length pfx)))
            (if (and (not (null? pfx)) (or (not len) (> l len)))
                (max-overlap rla rlb n l (+ n l))
                (max-overlap (cdr la) (cdr lb) beg len (+ n 1)))))))

  ;; Shift lists relative to each other and find maximum correlation (overlap).
  ;; Returns (values <a> <shifted b> <begin of overlap> <length of overlap>)
  (define (correlation a b [n (length a)])
    (let-values (((ob ol) (max-overlap a b)))
      (if (zero? n)
          (values a b ob ol)
          (let-values (((an bn obn oln)
                        (correlation a (shift-left b) (- n 1))))
            (if (or (not ol)
                    (and oln (> oln ol)))
                (values an bn obn oln)
                (values a b ob ol))))))

  ;; Make difference list
  (define (mark-diff a b)
    (foldr (lambda (ia ib out)
             (cond
              ((and (none? ia) (none? ib)) out)
              ((none? ia) (cons (cons (cons ia ib) 'b) out))
              ((none? ib) (cons (cons (cons ia ib) 'a) out))
              ((same? ia ib) (cons (cons (cons ia ib) 'both) out))
              (else (cons (cons (cons ia ib) 'diff) out))))
           '() a b))

  ;; diff-list function body
  (if (and (null? list-a) (null? list-b)) '()
      ;; Replace no item flag symbol to #f
      (map (lambda (x) (let ((ia (caar x))
                        (ib (cdar x))
                        (flag (cdr x)))
                    (cons (cond ((eq? flag 'a) (cons ia #f))
                                ((eq? flag 'b) (cons #f ib))
                                (else (cons ia ib)))
                          flag)))

           ;; Prepare lists for compare and shift
           (let* ((la (length list-a))
                  (lb (length list-b))
                  (a (append (make-list lb none) list-a (make-list lb none)))
                  (b (append list-b (make-list la none) (make-list lb none))))
             ;; Find longest continous overlap of lists
             (let-values (((a b ob ol) (correlation a b)))
               (if ob
                   ;; If found overlap split lists to 3 pieces -
                   ;; <before overlap> <overlap> <after overlap>
                   ;; and repeat diff for <before> and <after>
                   (let-values (((a1 a2 a3) (split3 a ob ol))
                                ((b1 b2 b3) (split3 b ob ol)))
                     (append (list-diff a1 b1 same?)
                             (mark-diff a2 b2)
                             (list-diff a3 b3 same?)))
                   ;; If no overlap simple mark difference
                   (mark-diff
                    (append list-a (make-list lb none))
                    (append list-b (make-list la none)))))))))
