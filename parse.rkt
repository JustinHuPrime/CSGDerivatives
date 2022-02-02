#lang typed/racket

(define-type Grammar grammar)
(struct grammar ([start-rule : Symbol]
                 [rules : (Listof rule)]))

(define-type Rule rule)
(struct rule ([left-context : Expansion]
              [nonterminal : Symbol]
              [right-context : Expansion]
              [expansion : Expansion]))
(struct start-rule rule ())

(define-type Expansion (Listof (U Symbol String)))

(define (expand-until-starts-with-token [token : String]
                                        [grammar : Grammar]
                                        [expansion : Expansion]
                                        [expanded : (Listof Rule)])
  : (Listof Expansion)
  (cond [(equal? (first expansion) token) (list expansion)]
        [(string? (first expansion)) empty]
        [else (let* ([candidate-rules (filter (λ ([rule : Rule])
                                                : Boolean
                                                (and (not (member rule expanded))
                                                     (empty? (rule-left-context rule))
                                                     (equal? (rule-nonterminal rule) (first expansion))
                                                     (>= (length expansion) (add1 (length (rule-right-context rule))))
                                                     (equal? (take (rest expansion) (length (rule-right-context rule)))
                                                             (rule-right-context rule))))
                                              (grammar-rules grammar))]
                     [expansions (map (λ ([rule : Rule])
                                        : (Pairof Expansion Rule)
                                        (cons (append (rule-expansion rule) (rest expansion))
                                              rule))
                                      candidate-rules)])
                (foldr (λ ([expansion : (Pairof Expansion Rule)] [rsf : (Listof Expansion)])
                         : (Listof Expansion)
                         (append (expand-until-starts-with-token token
                                                                 grammar
                                                                 (car expansion)
                                                                 (cons (cdr expansion) expanded))
                                 rsf))
                       empty
                       expansions))]))

(define (derivative [token : String]
                    [grammar : Grammar]
                    [expansion : Expansion])
  : (Listof (Pairof grammar (U Expansion False)))  
  (cond [(empty? expansion) (list (cons grammar #f))]
        [else (let ([starts-with-token (expand-until-starts-with-token token grammar expansion empty)])
                (list (cons grammar #f)))]))

; PSPACE-complete - probably exponential time in practice?
(define (recognizes? [grammar : Grammar]
                     [tokens : (Listof String)])
  : Boolean
  (let recurse ([tokens tokens]
                [grammar grammar]
                [expansion : Expansion (list (grammar-start-rule grammar))])
    (cond [(and (empty? expansion) (empty? tokens)) #t]
          [(or (empty? expansion) (empty? tokens)) #f]
          [else (ormap (λ ([derivative : (Pairof Grammar (U Expansion False))])
                         : Boolean
                         (match-let ([(cons grammar expansion) derivative])
                           (if (false? expansion)
                               #f
                               (recurse (rest tokens)
                                        grammar
                                        expansion))))
                       (derivative (first tokens) grammar expansion))])))

(module+ test
  (require typed/rackunit)

  (define anbncn (grammar 'S
                          (list (rule '() 'S '() '("a" B C))
                                (rule '() 'S '() '("a" S B C))
                                (rule '(C) 'B '() '(Z))
                                (rule '() 'C '(Z) '(W))
                                (rule '(W) 'Z '() '(C))
                                (rule '() 'W '(C) '(B))
                                (rule '("a") 'B '() '("b"))
                                (rule '("b") 'B '() '("b"))
                                (rule '("b") 'C '() '("c"))
                                (rule '("c") 'C '() '("c")))))

  (define addition (grammar 'S
                            (list (rule '() 'S '() '(S "+" S))
                                  (rule '() 'S '() '("1")))))

  (define addition+ (grammar 'S
                             (list (rule '() 'S '() '(S "+" S))
                                   (rule '() 'S '() '("1"))
                                   (rule '() 'S '("+" S "+" S) '("2")))))

  (test-equal? "expansion of S in a^n b^n c^n"
               (expand-until-starts-with-token "a" anbncn '(S) empty)
               (list '("a" B C)
                     '("a" S B C)))

  (test-equal? "expansion of S in addition"
               (expand-until-starts-with-token "1" addition '(S) empty)
               (list '("1" "+" S)
                     '("1")))

  (test-equal? "expansion of S in addition+"
               (expand-until-starts-with-token "2" addition '(S) empty)
               (list '("2" "+" S "+" S)))

  (test-true "a^n b^n c^n can be recognized"
             (recognizes? anbncn '("a" "a" "a" "b" "b" "b" "c" "c" "c")))

  (test-true "addition can be recognized"
             (recognizes? addition '("1" "+" "1" "+" "1"))))