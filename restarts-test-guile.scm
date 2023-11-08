;; Test driver for Guile.
(import (rnrs base (6))
        (srfi srfi-64)
        (guile)
        (restarts))

(include "restarts-test-body.scm")
