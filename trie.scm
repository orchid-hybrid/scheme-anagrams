;; A trie will be an assoc list where each entry is a trie
;; additionally '() is in there if it's a terminating node

(define (make-trie)
  '())

(define (prepare-argument s)
  (cond ((list? s) s)
        ((string? s) (string->list s))
        ((symbol? s) (string->list (symbol->string s)))
        (else (error "Can't use in trie:" s))))

(define (extend-trie t s)
  (extend-trie* t (prepare-argument s)))

(define (extend-trie* t s)
  (if (null? s)
      (add-terminator t)
      (add-association t (car s) (cdr s))))

(define (add-terminator t)
  (cond ((assoc '() t) => (lambda (entry) t))
        (else (cons (cons '() '()) t))))

(define (add-association t symbol rest)
  (cond ((assoc symbol t) =>
         (lambda (entry)
           (set-cdr! entry (extend-trie* (cdr entry) rest))
           t))
        (else (cons (cons symbol (extend-trie* (make-trie) rest)) t))))

(define (trie->list t) (trie->list* t '(())))

(define (trie->list* t acc)
  (define (flat-map f l) (apply append (map f l)))
  (flat-map (lambda (entry)
              (if (null? (car entry))
                  (map reverse acc)
                  (trie->list* (cdr entry)
                               (map (lambda (word) (cons (car entry) word))
                                    acc))))
            t))

(define (lookup-trie t s)
  (lookup-trie* t (prepare-argument s)))

(define (lookup-trie* t s)
  (if (null? s)
      t
      (cond ((assoc (car s) t) =>
             (lambda (entry)
               (lookup-trie* (cdr entry) (cdr s))))
            (else #f))))

(define (trie-contains? t s)
  (cond ((lookup-trie t s) =>
         (lambda (t*)
           (cond ((assoc '() t*) #t)
                 (else #f))))
        (else #f)))

