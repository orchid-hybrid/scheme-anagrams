(import (scheme base)
        (scheme write)
        (trie))

(let ((t (make-trie)))
  (set! t (extend-trie t "foo"))
  (set! t (extend-trie t "bar"))
  (set! t (extend-trie t "baz"))
  (set! t (extend-trie t "ba"))
  (set! t (extend-trie t "ba"))
  (set! t (extend-trie t "foom"))
  (set! t (extend-trie t ""))
  
  (display t)
  (newline)

  (display (trie->list t))
  (newline)
  
  '(for-each (lambda (word)
              (display word)
              (display (tree-lookup t word))
              ))
  )
