(define-library (trie)

  (import (scheme base) (scheme write))

  (export make-trie
          extend-trie
          trie->list
          lookup-trie
          trie-contains?)

  (include "trie.scm"))
