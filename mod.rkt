#lang at-exp racket/base

(require (for-syntax racket/base
                     syntax/parse)
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

(define/contract (->mod/simple v)
  (-> any/c mod?)
  (match v
    [(? symbol? s) (->mod/simple (~a s))] ;treat 'file.rkt as "file.rkt"
    [(or (? path? ap) (? path-string? ap))
     (let*-values ([(dir file _) (split-path (simplify-path ap))]
                   [(dir) (match dir ['relative (current-directory)][dir dir])])
       (mod file dir file))]
    [_ (mod #f (current-directory) #f)]))

(define/contract (->mod v)
  (-> any/c mod?)
  (define-match-expander mm
    ;; Match a value that, when applied to ->mod/simple, returns a mod
    ;; struct with non-#f module-path and filename; bind the fields.
    (syntax-parser
      [(_ mp:id dir:id file:id)
       #'(app ->mod/simple (mod (? values mp) dir (? values file)))]))
  (match v
    [(list 'submod (mm _  d f) (? symbol? ss) ..1) (mod (list* 'submod f ss) d f)]
    [(list         (mm _  d f) (? symbol? ss) ..1) (mod (list* 'submod f ss) d f)]
    [(list         (mm mp d f))                    (mod mp d f)]
    [(mm mp d f)                                   (mod mp d f)]
    [_                                             (mod #f (current-directory) #f)]))

(module+ test
  (require rackunit)
  (define-syntax-rule (= x y) (check-equal? x y))
  (define f.rkt (string->path "f.rkt"))
  ;; rel path
  (let ([dir (current-directory)])
    (= (->mod "f.rkt") (mod f.rkt dir f.rkt))
    (= (->mod 'f.rkt)  (mod f.rkt dir f.rkt))
    (= (->mod '(submod "f.rkt" a b)) (mod `(submod ,f.rkt a b) dir f.rkt))
    (= (->mod '(submod f.rkt a b))   (mod `(submod ,f.rkt a b) dir f.rkt))
    (= (->mod '("f.rkt" a b)) (mod `(submod ,f.rkt a b) dir f.rkt))
    (= (->mod '(f.rkt a b))   (mod `(submod ,f.rkt a b) dir f.rkt))
    (= (->mod '("f.rkt")) (mod f.rkt dir f.rkt))
    (= (->mod '(f.rkt))   (mod f.rkt dir f.rkt)))
  ;; abs path
  (let ([dir (string->path "/p/t/")])
    (= (->mod "/p/t/f.rkt") (mod f.rkt dir f.rkt))
    (= (->mod '/p/t/f.rkt)  (mod f.rkt dir f.rkt))
    (= (->mod '(submod "/p/t/f.rkt" a b)) (mod `(submod ,f.rkt a b) dir f.rkt))
    (= (->mod '(submod /p/t/f.rkt a b))   (mod `(submod ,f.rkt a b) dir f.rkt))
    (= (->mod '("/p/t/f.rkt" a b)) (mod `(submod ,f.rkt a b) dir f.rkt))
    (= (->mod '(/p/t/f.rkt a b))   (mod `(submod ,f.rkt a b) dir f.rkt))
    (= (->mod '("/p/t/f.rkt")) (mod f.rkt dir f.rkt))
    (= (->mod '(/p/t/f.rkt))   (mod f.rkt dir f.rkt)))
  ;; nonsense input => (mod #f (current-directory) #f)
  (let ([nothing (mod #f (current-directory) #f)])
    (= (->mod 42)                nothing)
    (= (->mod '(42 'bar))        nothing)
    (= (->mod '(submod 42 'bar)) nothing)
    (= (->mod '(submod (submod "f.rkt" foo) bar)) nothing)))

(define/contract (->mod/existing v)
  (-> any/c mod?)
  (match (->mod v)
    [(and v (mod #f dir #f)) v]
    [(and v (mod mp dir file))
     (define path (build-path dir file))
     (cond [(and mp file (file-exists? path)) v]
           [else (display-commented (format "~a does not exist" path))
                 (mod #f dir #f)])]))

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
