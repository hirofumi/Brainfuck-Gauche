(define-syntax brainfuck
  (syntax-rules ()
    ((_ (#\> . program) history in out before char (next . after))
     (brainfuck program (#\> . history) in out (char . before) next after))
    ((_ (#\> . program) history in out before char ())
     (brainfuck program (#\> . history) in out (char . before) () ()))
    ((_ (#\< . program) history in out (prev . before) char after)
     (brainfuck program (#\< . history) in out before prev (char . after)))
    ((_ (#\< . program) history in out () char after)
     (brainfuck program (#\< . history) in out () () (char . after)))
    ((_ (#\+ . program) history in out before
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
        after)
     (brainfuck program (#\+ . history) in out before () after))
    ((_ (#\+ . program) history in out before char after)
     (brainfuck program (#\+ . history) in out before (0 . char) after))
    ((_ (#\- . program) history in out before () after)
     (brainfuck program (#\- . history) in out before
                (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                after))
    ((_ (#\- . program) history in out before (0 . char) after)
     (brainfuck program (#\- . history) in out before char after))
    ((_ (#\. . program) history in (out ...) before char after)
     (brainfuck program (#\. . history) in (out ... char) before char after))
    ((_ (#\, . program) history (in-char . in) out before char after)
     (brainfuck program (#\, . history) in out before in-char after))
    ((_ (#\, . program) history () out before char after)
     (brainfuck program (#\, . history) in out before () after))
    ((_ (#\[ . program) history in out before () after)
     (brainfuck "{" () program (#\[ . history) in out before () after))
    ((_ (#\[ . program) history in out before char after)
     (brainfuck program (#\[ . history) in out before char after))
    ((_ (#\] . program) history in out before () after)
     (brainfuck program (#\] . history) in out before () after))
    ((_ (#\] . program) history in out before char after)
     (brainfuck "}" () (#\] . program) history in out before char after))
    ((_ (unknown . program) history in out before char after)
     (brainfuck program history in out before char after))
    ((_ () history in out before char after)
     'out)
    ((_ "{" count (#\[ . program) history . rest)
     (brainfuck "{" (0 . count) program (#\[ . history) . rest))
    ((_ "{" (0 . count) (#\] . program) history . rest)
     (brainfuck "{" count program (#\] . history) . rest))
    ((_ "{" () (#\] . program) history . rest)
     (brainfuck program (#\] . history) . rest))
    ((_ "{" count (op . program) history . rest)
     (brainfuck "{" count program (op . history) . rest))
    ((_ "}" count program (#\] . history) . rest)
     (brainfuck "}" (0 . count) (#\] . program) history . rest))
    ((_ "}" (0 . count) program (#\[ . history) . rest)
     (brainfuck "}" count (#\[ . program) history . rest))
    ((_ "}" () program (#\[ . history) . rest)
     (brainfuck program (#\[ . history) . rest))
    ((_ "}" count program (op . history) . rest)
     (brainfuck "}" count (op . program) history . rest))
    ((_ program)
     (brainfuck program ()))
    ((_ program in)
     (brainfuck program () in () () () ()))))

(define-macro (brainfuck-friendly source . in)
  `(for-each (compose display integer->char length)
             (brainfuck ,(string->list source)
                        ,(map (compose make-list char->integer)
                              (if (pair? in) (string->list (car in)) '())))))

;; http://en.wikipedia.org/wiki/Hello_world_program_examples#brainfuck
(brainfuck-friendly
 "++++++++++[>+++++++>++++++++++>+++>+<<<<-]
  >++.>+.+++++++..+++.>++.<<++++++++++ +++++.>.+++.------.--------.>+.>.")
