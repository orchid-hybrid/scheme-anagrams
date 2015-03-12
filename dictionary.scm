(define (load-dictionary)
  (with-input-from-file "words.txt"
    (lambda ()
      (let loop ((t (make-trie)))
        (let ((word (read-line)))
          (if (eof-object? word)
              t
              (loop (extend-trie t word))))))))

