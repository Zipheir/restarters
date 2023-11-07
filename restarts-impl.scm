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
  (cond
   ((list? restarters)
    (let loop ((elements restarters)
               (tags '()))
      (cond
       ((null? elements) restarters)
       (else (let* ((r (car elements)))
               (unless (restarter? r)
                 (error "not a restarter in list" t))
               (when (and (restarter-tag r)
                          (find (lambda (t)
                                  (eqv? t (restarter-tag r)))
                                tags))
                 (error "duplicate tag in restarter list" (restarter-tag r)))
               (loop (cdr elements)
                     (cons (restarter-tag r)
                           tags)))))))
   ((restarter? restarters)
    (list restarters))
   ((and allow-compound? (compound? restarters))
    (let* ((subobjs (compound-subobjects restarters))
           (restarters (filter restarter? subobjs)))
      restarters))
   (else (if allow-compound?
             (error "not a restarter, list of restarters or a compound object" restarters)
             (error "not a restarter or a list of restarters" restarters)))))

(define (with-restarters restarters thunk)
  (parameterize ((ambient-restarters (collect-restarters restarters)))
    (thunk)))

(define (make-restarter-tag-pred tag)
  (lambda (restarter)
    (eqv? tag (restarter-tag restarter))))

(define (find-restarter tag restarters)
  (let ((pred (make-restarter-tag-pred tag)))
    (or (find pred (restarters->list restarters #t))
        (find pred (ambient-restarters)))))

(define (collect-restarters restarters)
  (define lst (append (restarters->list restarters #t)
                      (ambient-restarters)))
  (let loop ((lst lst)
             (rez '())
             (tags '()))
    (cond
     ((null? lst) (reverse rez))
     (else (let* ((restarter (car lst))
                  (tag (restarter-tag restarter)))
             (if (or (not tag)
                     (find (lambda (t) (eqv? tag t)) tags))
                 (loop (cdr lst)
                       rez
                       tags)
                 (loop (cdr lst)
                       (cons restarter rez)
                       (cons tag tags))))))))

(define (restart-interactively restarters)
  ((interactor) (collect-restarters restarters)))

(define (default-interactor restarters)

  (define l (length restarters))

  (define (display-choices)
    (display "Choose restarter:\n")
    (for-each
     (lambda (r index)
       (display "\t")
       (display (+ 1 index))
       (display ". ")
       (display (restarter-tag r))
       (display " ")
       (display (car (restarter-description r)))
       (newline))
     restarters
     (iota l)))

  (define (read-choice)
    (define choice (read))
    (if (<= 1 choice l)
        (list-ref restarters (- choice 1))
        (begin
           (display "Choice must be a number between 1 and ")
           (display l)
           (newline)
           (read-choice))))

  (define (read-restarter-params restarter)
    (let loop ((descriptions (cdr (restarter-description restarter)))
               (param-values '()))
      (cond
       ((null? descriptions) (reverse param-values))
       (else
        (begin
          (display (car descriptions))
          (newline)
          (loop (cdr descriptions)
                (cons (read) param-values)))))))

  (display-choices)
  (let* ((restarter (read-choice))
         (params (read-restarter-params restarter)))
    (apply restart restarter params)))

(define interactor (make-parameter default-interactor))
