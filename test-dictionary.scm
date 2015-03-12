(import (scheme base)
        (scheme write)
        (trie) (dictionary))

(define dict (load-dictionary))

(display (map (lambda (rest) (append (string->list "pon") rest)) (trie->list (lookup-trie dict "pon"))))

(let loop ()
  (display "Enter a word") (newline)
  (let ((word (read-line)))
    (unless (eof-object? word)
            (if (trie-contains? dict word)
                (begin (display "yes") (newline))
                (begin (display "no") (newline)))
            (loop))))
