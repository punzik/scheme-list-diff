#lang racket

(require "list-diff.rkt")

;;; TEST
(define (esc str) (string-append (list->string (list (integer->char 27))) str))
(define bg-black           (esc "[40m"))
(define bg-red             (esc "[41m"))
(define bg-green           (esc "[42m"))
(define bg-yellow          (esc "[43m"))

(define (print-string-diff s1 s2)
  (let ((diff (list-diff (string->list s1)
                         (string->list s2)
                         char-ci=?)))
    (for ((x diff))
      (let ((a (car x))
            (b (cdr x)))
        (cond
         ((not a) (printf "~a~a" bg-red " "))
         ((not b) (printf "~a~a" bg-green a))
         ((char-ci=? a b) (printf "~a~a" bg-black a))
         (else (printf "~a~a" bg-yellow a)))))
    (printf "~a\n" bg-black)

    (for ((x diff))
      (let ((a (car x))
            (b (cdr x)))
        (cond
         ((not b) (printf "~a~a" bg-red " "))
         ((not a) (printf "~a~a" bg-green b))
         ((char-ci=? a b) (printf "~a~a" bg-black b))
         (else (printf "~a~a" bg-yellow b)))))
    (printf "~a\n" bg-black)))

(define (main)
  (let ((cmdl (current-command-line-arguments)))
    (if (< (vector-length cmdl) 2)
        (printf "Where is string?\n")
        (print-string-diff (vector-ref cmdl 0)
                           (vector-ref cmdl 1)))))

(main)
