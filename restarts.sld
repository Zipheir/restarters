(define-library
    (restarts)
    (cond-expand
        ((library (srfi 222)) (import (scheme base)
                                      (scheme list)
                                      (scheme write)
                                      (scheme read)
                                      (srfi 222)))
        (else (import (scheme base)
                      (scheme list)
                      (scheme write)
                      (scheme read))
              (begin
                (define (compound? obj) #f)
                (define (compound-subobjects obj) '()))))
    (export
        make-restarter
        restarter?
        restarter-tag
        restarter-description

        restart
        ambient-restarters
        with-restarter
        find-restarter
        collect-restarters
        restart-interactively
        interactor)
    (include "restarts-impl.scm"))
