#lang at-exp racket/base

(require (for-syntax racket/base)
         racket/contract/base
         racket/contract/region
         racket/format
         racket/match
         racket/string
         syntax/location
         "util.rkt")

(provide (struct-out mod)
         ->mod/existing
         mod->prompt-string
         maybe-warn-about-submodules)

(define-struct/contract mod
  ([path (or/c #f module-path?)]
   [dir  absolute-path?]
   [file (or/c #f relative-path?)])
  #:transparent)

(define/contract (->mod v)
  (-> any/c mod?)
  (define cd (current-directory))
  (match v
    [(? symbol? s) (->mod (~a s))] ;convenience treat file.rkt as "file.rkt"
    [(? rel-path? rp)
     (define p (string->path rp))
     (mod p cd p)]
    [(? abs-path? ap)
     (define-values (base name _) (split-path (simplify-path ap)))
     (mod name base name)]
    [(list 'submod
           (app ->mod (mod (? values) dir (? values file)))
           (? symbol? ss) ..1)
     (mod (list* 'submod file ss) dir file)]
    [(list (app ->mod (mod (? values) dir (? values file)))
           (? symbol? ss) ..1)
     (mod (list* 'submod file ss) dir file)]
    [_ (mod #f cd #f)]))

(module+ test
  (require rackunit)
  (define foo.rkt (string->path "foo.rkt"))
  ;; rel path
  (let ([dir (current-directory)])
    (check-equal? (->mod "foo.rkt")
                  (mod foo.rkt dir foo.rkt))
    (check-equal? (->mod 'foo.rkt)
                  (mod foo.rkt dir foo.rkt))
    (check-equal? (->mod '(submod "foo.rkt" a b))
                  (mod `(submod ,foo.rkt a b) dir foo.rkt))
    (check-equal? (->mod '(submod foo.rkt a b))
                  (mod `(submod ,foo.rkt a b) dir foo.rkt))
    (check-equal? (->mod '("foo.rkt" a b))
                  (mod `(submod ,foo.rkt a b) dir foo.rkt))
    (check-equal? (->mod '(foo.rkt a b))
                  (mod `(submod ,foo.rkt a b) dir foo.rkt)))
  ;; abs path
  (let ([dir (string->path "/path/to/")])
    (check-equal? (->mod "/path/to/foo.rkt")
                  (mod foo.rkt dir foo.rkt))
    (check-equal? (->mod '/path/to/foo.rkt)
                  (mod foo.rkt dir foo.rkt))
    (check-equal? (->mod '(submod "/path/to/foo.rkt" a b))
                  (mod `(submod ,foo.rkt a b) dir foo.rkt))
    (check-equal? (->mod '(submod /path/to/foo.rkt a b))
                  (mod `(submod ,foo.rkt a b) dir foo.rkt))
    (check-equal? (->mod '("/path/to/foo.rkt" a b))
                  (mod `(submod ,foo.rkt a b) dir foo.rkt))
    (check-equal? (->mod '(/path/to/foo.rkt a b))
                  (mod `(submod ,foo.rkt a b) dir foo.rkt)))
  ;; nonsense input => (mod #f (current-directory) #f)
  (let ([nothing (mod #f (current-directory) #f)])
    (check-equal? (->mod 42) nothing)
    (check-equal? (->mod '(submod 42 'bar)) nothing)
    (check-equal? (->mod '(42 'bar)) nothing)))

(define/contract (->mod/existing v)
  (-> any/c mod?)
  (match (->mod v)
    [(and v (mod #f dir #f)) v]
    [(and v (mod mp dir file))
     (define path (build-path dir file))
     (cond [(and mp file (file-exists? path)) v]
           [else (display-commented (format "~a does not exist" path))
                 (mod #f dir #f)])]))

;; Actual predicates that accept any/c:
(define (rel-path? v) (and (or (path-for-some-system? v) (path-string? v))
                           (relative-path? v)))
(define (abs-path? v) (and (or (path-for-some-system? v) (path-string? v))
                           (absolute-path? v)))

(define (mod->prompt-string m)
  (match (mod-path m)
    [#f                 ""]
    [(? path? p)        (~a p)]
    [(list* 'submod xs) (string-join (map ~a xs) ":")]))

;; Check whether Racket is new enough (newer than 6.2.1) that
;; module->namespace works with module+ and (module* _ #f __)
;; forms when errortrace is enabled.
(module+ check
  (define x 42))
(define can-enter-module+-namespace?
  (with-handlers ([exn:fail? (λ _ #f)])
    (define mp (quote-module-path check))
    (dynamic-require mp)
    (eval 'x (module->namespace mp))
    #t))

(define warned? #f)
(define/contract (maybe-warn-about-submodules mp context)
  (-> (or/c #f module-path?) symbol? any)
  (when (pair? mp) ;submodule?
    (unless (or warned?
                can-enter-module+-namespace?
                (memq context '(low medium)))
      (set! warned? #t)
      (display-commented
       @~a{Note: The submodule @mp will be evaluated.
                 However your Racket version is old. You will be unable to
                 use the REPL to examine definitions in the body of a module+
                 or (module _ #f ___) form when errortrace is enabled. Set the
                 Emacs variable racket-error-context to 'low or 'medium, or,
                 upgrade Racket.}))))
