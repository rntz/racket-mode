#lang racket/base

(require (only-in errortrace/errortrace-key errortrace-key)
         errortrace/stacktrace
         racket/unit)

(provide make-debug-eval-handler
         profiling-enabled
         test-coverage-enabled
         test-coverage-info)

;;; Better stack traces ("basic errortrace")

;; a member of stacktrace-imports^
(define (with-mark src-stx expr phase)
  (define source (cond [(path? (syntax-source src-stx))
                        (syntax-source src-stx)]
                       [else #f]))
  (define position (or (syntax-position src-stx) 0))
  (define span (or (syntax-span src-stx) 0))
  (define line (or (syntax-line src-stx) 0))
  (define column (or (syntax-column src-stx) 0))
  (cond [source (with-syntax
                  ([expr expr]
                   [mark (list 'dummy-thing source line column position span)]
                   [wcm (syntax-shift-phase-level #'with-continuation-mark phase)]
                   [errortrace-key errortrace-key]
                   [qte (syntax-shift-phase-level #'quote phase)])
                  (syntax (wcm (qte errortrace-key)
                               (qte mark)
                               expr)))]
        [else expr]))

;; ;; cms->srclocs : continuation-marks -> (listof srcloc)
;; (define (cms->srclocs cms)
;;   (map
;;    (λ (x) (make-srcloc (list-ref x 1)
;;                        (list-ref x 2)
;;                        (list-ref x 3)
;;                        (list-ref x 4)
;;                        (list-ref x 5)))
;;    (continuation-mark-set->list cms errortrace-key)))

(define ((make-debug-eval-handler orig-eval) orig-exp)
  (cond
    [(compiled-expression? (if (syntax? orig-exp)
                               (syntax-e orig-exp)
                               orig-exp))
     (orig-eval orig-exp)]
    [else
     (let loop ([exp (if (syntax? orig-exp)
                         orig-exp
                         (namespace-syntax-introduce (datum->syntax #f orig-exp)))])
       (let ([top-e (expand-syntax-to-top-form exp)])
         (syntax-case top-e (begin)
           [(begin expr ...)
            ;; Found a `begin', so expand/eval each contained
            ;; expression one at a time
            (let i-loop ([exprs (syntax->list #'(expr ...))]
                         [last-one (list (void))])
              (cond
                [(null? exprs)
                 (apply values last-one)]
                [else
                 (i-loop (cdr exprs)
                         (call-with-values
                          (λ ()
                            (call-with-continuation-prompt
                             (λ () (loop (car exprs)))
                             (default-continuation-prompt-tag)
                             (λ args
                               (apply
                                abort-current-continuation
                                (default-continuation-prompt-tag)
                                args))))
                          list))]))]
           [_else
            ;; Not `begin', so proceed with normal expand and eval
            (let ([annotated (annotate-top (expand-syntax top-e)
                                           (namespace-base-phase))])
              ;; (local-require racket/pretty)
              ;; (pretty-write (list (syntax->datum top-e)
              ;;                     "=>"
              ;;                     (syntax->datum (expand-syntax top-e))
              ;;                     "=>"
              ;;                     (syntax->datum annotated)))
              (orig-eval annotated))])))]))

;;; Test coverage

;; a member of stacktrace-imports^
(define test-coverage-enabled (make-parameter #f))

(define test-coverage-info (make-hasheq))

(define (clear-test-coverage-info!)
  (set! test-coverage-info (make-hasheq)))

;; a member of stacktrace-imports^
(define (initialize-test-coverage-point expr)
  (hash-set! test-coverage-info expr (mcons #f #f)))

;; a member of stacktrace-imports^
(define (test-covered expr)
  (define v (hash-ref test-coverage-info expr #f))
  (with-syntax ([v v])
    #'(#%plain-app set-mcar! v #t)))

;;; Profiling

;; a member of stacktrace-imports^
(define profile-key (gensym))

;; a member of stacktrace-imports^
(define profiling-enabled (make-parameter #f))

(define profile-info (make-hasheq))

(struct prof
  (nest? ;guard nested calls
   num   ;exact-nonnegative-integer?
   time  ;exact-nonnegative-integer?
   name  ;(or/c #f symbol?)
   expr) ;syntax?
  #:mutable
  #:transparent)

;; a member of stacktrace-imports^
(define (initialize-profile-point key name expr)
  (hash-set! profile-info
             key
             (prof #f 0 0 (and (syntax? name) (syntax-e name)) expr)))

;; a member of stacktrace-imports^
(define (register-profile-start key)
  (define p (hash-ref profile-info key))
  (set-prof-num! p (add1 (prof-num p)))
  (cond [(prof-nest? p) #f]
        [else (set-prof-nest?! p #t)
              (current-process-milliseconds)]))

;; a member of stacktrace-imports^
(define (register-profile-done key start)
  (void
   (when start
     (define p (hash-ref profile-info key))
     (set-prof-nest?! p #f)
     (set-prof-time! p (+ (- (current-process-milliseconds) start)
                          (prof-time p))))))

;;; Finally, invoke the unit
(define-values/invoke-unit/infer stacktrace@)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; example

(parameterize ([test-coverage-enabled #t]
               [profiling-enabled #t]
               [current-eval (make-debug-eval-handler (current-eval))])
  (namespace-require (string->path "/tmp/foo.rkt")))
test-coverage-info
profile-info
