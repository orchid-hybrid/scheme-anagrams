(import (scheme base)
        (scheme write)
        (rnrs sorting (6))
        (trie) (dictionary))

(define dict (load-dictionary))

(define (remove-spaces list)
  (filter (lambda (char) (not (eq? char #\space))) list))

(define (group l)
  (define (group-worker hd g tl)
    (if (null? tl)
        (list g)
        (if (eq? hd (car tl))
            (group-worker (car tl) (cons hd g) (cdr tl))
            (cons g (group-worker (car tl) (list (car tl)) (cdr tl))))))
  (if (null? l)
      '()
      (group-worker (car l) (list (car l)) (cdr l))))

(define (prepare line)
  (group (list-sort char<? (remove-spaces (string->list line)))))

(define (find-anagrams letters)
  (anagrams '() '() dict letters)
  (newline))

;;

(define (select letters)
  (define (select-car letters)
    (let* ((group (car letters))
           (letter (car group))
           (rest (cdr letters)))
      (if (null? (cdr group))
          (cons letter rest)
          (cons letter (cons (cdr group) rest)))))
  (if (null? letters)
      '()
      (cons (select-car letters)
            (map (lambda (rest)
                   (cons (car rest)
                         (cons (car letters) (cdr rest))))
                 (select (cdr letters))))))

(define (anagrams sentence prefix trie letters)
  (when (trie-contains? trie '())
        (when (null? letters)
              (display (list 'sentence! (map list->string (cons (reverse prefix) sentence))))
              (newline))
        (anagrams (cons (reverse prefix) sentence) '() dict letters))
  (for-each (lambda (option)
              (let* ((letter (car option))
                     (letters* (cdr option))
                     (trie* (lookup-trie trie (list letter))))
                (when trie*
                      (anagrams sentence (cons letter prefix) trie* letters*))))
            (select letters)))

;;

(let loop ()
  (display "Enter a word") (newline)
  (let ((line (read-line)))
    (unless (eof-object? line)
            (find-anagrams (prepare line))
            (loop))))
