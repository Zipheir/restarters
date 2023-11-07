(define-syntax assert-type
  ((assert-type test . args)
   (unless test
     (error "type check failed" 'expr . args))))

(define-record-type <restarter>
  (make-raw-restarter tag description invoker)
  restarter?
  (tag restarter-tag)
  (description restarter-description)
  (invoker restarter-invoker))

;; Exported constructor.
(define (make-restarter tag description invoker)
  (assert-type (symbol? tag))
  (assert-type (and (pair? description)
                    (every string? description)))
  (assert-type (procedure? invoker))
  (make-raw-restarter tag description invoker))

(define (restart restarter . args)
  (apply (restarter-invoker restarter) args))

(define ambient-restarters (make-parameter '()))

(define (restarters->list restarters allow-compound?)
  ;; Ensure that each element of *restarters* is a
  ;; restarter with a unique tag (with respect to the
  ;; rest of the list).
  (define (check-restarter-list restarters)
    (let loop ((elts restarters) (seen-tags '()))
      (when (pair? elts)
        (let* ((r (car elts)) (tag (restarter-tag r)))
          (assert-type (restarter? r) r)
          (when (memv tag seen-tags)
            (error "duplicate tag in restarter list" tag))
          (loop (cdr elts) (cons tag seen-tags))))))

  (cond
   ((list? restarters)
    (check-restarter-list restarters)
    restarters)
   ((restarter? restarters) (list restarters))
   ((and allow-compound? (compound? restarters))
    (let* ((subobjs (compound-subobjects restarters))
           (restarters (filter restarter? subobjs)))
      restarters))
   (allow-compound?
    (error "not a restarter, list of restarters, or a compound object"
           restarters))
   (else
    (error "not a restarter or a list of restarters" restarters))))

(define (with-restarters restarters thunk)
  (parameterize ((ambient-restarters (collect-restarters restarters)))
    (thunk)))

(define (find-restarter tag restarters)
  (let ((pred (lambda (restarter)
                (eqv? tag (restarter-tag restarter)))))
    (or (find pred (restarters->list restarters #t))
        (find pred (ambient-restarters)))))

(define (collect-restarters restarters)
  (let ((list (restarters->list restarters #t)))
    (append list
            (lset-difference (lambda (r1 r2)
                               (eqv? (restarter-tag r1)
                                     (restarter-tag r2)))
                             (ambient-restarters)
                             list))))

(define (restart-interactively restarters)
  ((interactor) (collect-restarters restarters)))

(define (default-interactor restarters)
  ;; Like find-restarter, but only searches its argument list.
  (define (find-local-restarter tag rs)
    (find (lambda (r) (eqv? tag (restarter-tag r))) rs))

  (define (display-choices)
    (display "The following actions are available:")
    (newline)
    (for-each
     (lambda (r)
       (display (restarter-tag r))
       (display ": ")
       (display (car (restarter-description r)))
       (newline))
     restarters)
    (display "Which action do you choose: "))

  (define (read-choice)
    (let ((choice (read)))
      (or (find-local-restarter choice restarters)
          (begin
           (display "Invalid choice. Try again.")
           (newline)
           (read-choice)))))

  (define (read-restarter-params restarter)
    (unfold null?
            (lambda (ds)
              (begin
               (display (car ds))
               (display ": ")
               (read)))
            cdr
            (restarter-description restarter)))

  (display-choices)
  (let* ((restarter (read-choice))
         (params (read-restarter-params restarter)))
    (apply restart restarter params)))

(define interactor (make-parameter default-interactor))
