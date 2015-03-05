#lang racket/base

(require (only-in errortrace/errortrace-key errortrace-key)
         errortrace/stacktrace
         racket/match
         racket/unit)

(provide make-instrumented-eval-handler
         print-error-trace
         error-context-display-depth
         instrumenting-enabled
         test-coverage-enabled
         clear-test-coverage-info!
         get-test-coverage-info
         profiling-enabled
         clear-profile-info!
         get-profile-info)

;;; Core instrumenting

(define instrumenting-enabled (make-parameter #f))

(define ((make-instrumented-eval-handler orig-eval) orig-exp)
  (cond
    [(or (not (instrumenting-enabled))
         (compiled-expression? (if (syntax? orig-exp)
                                   (syntax-e orig-exp)
                                   orig-exp)))
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

;;; Better stack traces ("basic errortrace")

(define base-phase
  (variable-reference->module-base-phase (#%variable-reference)))

(define (with-mark src-stx expr phase) ;stacktrace-imports^
  (define source (cond [(path? (syntax-source src-stx))
                        (syntax-source src-stx)]
                       [else #f]))
  (define position (or (syntax-position src-stx) 0))
  (define span (or (syntax-span src-stx) 0))
  (define line (or (syntax-line src-stx) 0))
  (define column (or (syntax-column src-stx) 0))
  (define phase-shift (- phase base-phase))
  (cond [source
         (with-syntax
           ([expr expr]
            [mark (list 'dummy source line column position span)]
            [wcm (syntax-shift-phase-level #'with-continuation-mark phase-shift)]
            [errortrace-key errortrace-key]
            [qte (syntax-shift-phase-level #'quote phase-shift)])
           (syntax (wcm (qte errortrace-key)
                        (qte mark)
                        expr)))]
        [else expr]))

(define error-context-display-depth
  (make-parameter 10000
                  (λ (x) (and (integer? x) x))))

(define (print-error-trace port exn)
  (for ([_ (in-range (error-context-display-depth))]
        [cm (in-list (continuation-mark-set->list (exn-continuation-marks exn)
                                                  errortrace-key))])
    (match-define (list datum source line col pos span) cm)
    (define file (cond [(string? source) source]
                       [(path? source)
                        (path->string source)]
                       [(not source)
                        #f]
                       [else
                        (format "~a" source)]))
    (fprintf port
             "\n   ~a~a: ~.s"
             (or file "[unknown source]")
             (cond [line (format ":~a:~a" line col)]
                   [pos (format "::~a" pos)]
                   [else ""])
             datum)))


;;; Test coverage

(define test-coverage-enabled (make-parameter #f)) ;stacktrace-imports^

(define test-coverage-info (make-hasheq))

(define (clear-test-coverage-info!)
  (hash-clear! test-coverage-info))

(define (initialize-test-coverage-point expr) ;stacktrace-imports^
  (hash-set! test-coverage-info expr (mcons #f #f)))

(define (test-covered expr) ;stacktrace-imports^
  (define v (hash-ref test-coverage-info expr #f))
  (and v (with-syntax ([v v])
           #'(#%plain-app set-mcar! v #t))))

(define (get-test-coverage-info)
  ;; Due to macro expansion (e.g. to an `if` form), there may be
  ;; multiple data points for the exact same source location. We want
  ;; to logically OR them: If any are true, the source location is
  ;; covered.
  (define ht (make-hash)) ;; (list src pos span) => cover?
  (for* ([(stx v) (in-hash  test-coverage-info)]
         [cover?  (in-value (mcar v))]
         [loc     (in-value (list (syntax-source stx)
                                  (syntax-position stx)
                                  (syntax-span stx)))])
    (match (hash-ref ht loc 'none)
      ['none (hash-set! ht loc cover?)]
      [#f    (when cover? (hash-set! ht loc #t))]
      [#t    (void)]))
  (for/list ([(loc cover?) (in-hash ht)])
    (cons cover? loc)))

;;; Profiling

(define profile-key (gensym)) ;stacktrace-imports^

(define profiling-enabled (make-parameter #f)) ;stacktrace-imports^

(define profile-info (make-hasheq))

(define (clear-profile-info!)
  (hash-clear! profile-info))

(struct prof
  (nest? ;guard nested calls
   num   ;exact-nonnegative-integer?
   time  ;exact-nonnegative-integer?
   name  ;(or/c #f symbol?)
   expr) ;syntax?
  #:mutable
  #:transparent)

(define (initialize-profile-point key name expr) ;stacktrace-imports^
  (hash-set! profile-info
             key
             (prof #f 0 0 (and (syntax? name) (syntax-e name)) expr)))

(define (register-profile-start key) ;stacktrace-imports^
  (define p (hash-ref profile-info key))
  (set-prof-num! p (add1 (prof-num p)))
  (cond [(prof-nest? p) #f]
        [else (set-prof-nest?! p #t)
              (current-process-milliseconds)]))

(define (register-profile-done key start) ;stacktrace-imports^
  (void
   (when start
     (define p (hash-ref profile-info key))
     (set-prof-nest?! p #f)
     (set-prof-time! p (+ (- (current-process-milliseconds) start)
                          (prof-time p))))))

(define (get-profile-info)
  (for/list ([x (in-list (hash-values profile-info))])
    (match-define (prof nest? count msec name stx) x)
    (list count msec name stx)))


;;; Finally, invoke the unit
(define-values/invoke-unit/infer stacktrace@)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; example

;; (parameterize ([instrumenting-enabled #t]
;;                [test-coverage-enabled #t]
;;                [profiling-enabled #f]
;;                [current-eval (make-instrumented-eval-handler (current-eval))])
;;   (namespace-require (string->path "/tmp/foo.rkt")))
;; (get-test-coverage-info)
;; (get-profile-info)
