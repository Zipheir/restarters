(cond-expand
  (guile
   (import (scheme base)
           (restarts)
           (srfi srfi-64)))
  (chibi
   (import (scheme base)
           (restarts)
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (scheme base)
           (restarts)
           (srfi 64))))


(test-begin "Restarts")

(test-group
 "restart"

 (define restarter (make-restarter #f '("test") (lambda args (cons 'test args))))
 (test-equal '(test 1 2 3) (restart restarter 1 2 3)))

(test-group
 "with-restarter, find-restarter, collect-restarters"

 (define a1 (make-restarter 'r1 '("test") (lambda args args)))
 (define a2 (make-restarter 'r2 '("test") (lambda args args)))
 (define a3 (make-restarter #f '("test") (lambda args args)))
 (define r1 (make-restarter 'r1 '("test") (lambda args args)))
 (define r2 (make-restarter 'r2 '("test") (lambda args args)))

 (with-restarter a1
                 (lambda ()
                   (test-eq a1 (find-restarter 'r1 '()))))

 (with-restarter a1
                 (lambda ()
                   (test-eq r1 (find-restarter 'r1 r1))))

 (with-restarter (list a1 a2)
                 (lambda ()
                   (test-equal #f (find-restarter 'r3 (list r1 r2)))))

 (with-restarter (list a1 a2)
                 (lambda ()
                   (define collected (collect-restarters (list r1)))
                   (test-eq r1 (car collected))
                   (test-eq a2 (cadr collected))
                   (test-assert (null? (cddr collected))))))

(test-group
 "default interactor"

 (define a1 (make-restarter 'a1 '("ambient1") (lambda args (error "shouldn't get here"))))
 (define r1 (make-restarter 'r1 '("restarter1" "param") (lambda (arg) arg)))
 (define input-port (open-input-string "3 1 foo")) ;; choose (invalidly) 3rd restarter, choose 1st restarter, supply parameter
 (define expected-output
   (string-append
    "Choose restarter:\n"
    "\t1. r1 restarter1\n"
    "\t2. a1 ambient1\n"
    "Choice must be a number between 1 and 2\n"
    "param\n"))
 (define output-port (open-output-string))
 (define result
   (parameterize ((current-input-port input-port)
                  (current-output-port output-port))
     (with-restarter a1
                     (lambda ()
                       (restart-interactively r1)))))

 (test-equal 'foo result)
 (test-equal expected-output (get-output-string output-port)))

(test-end)
