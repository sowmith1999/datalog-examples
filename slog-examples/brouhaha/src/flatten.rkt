#lang racket

;; I have no idea what I am going to write in this blank file
(require print-debug/print-dbg)
(require racket/string)
(define input
  '(store (f-addr [] "ack")
          (define "ack"
            (fixedparam ["m" "n"])
            (if (app (ref "=") [(ref "m") (int "0")])
                (app (ref "+") [(ref "n") (int "1")])
                (if (app (ref "=") [(ref "n") (int "0")])
                    (app (ref "ack") [(app (ref "-") [(ref "m") (int "1")]) (int "1")])
                    (app (ref "ack")
                         [(app (ref "-") [(ref "m") (int "1")])
                          (app (ref "ack") [(ref "m") (app (ref "-") [(ref "n") (int "1")])])]))))))

(define to-save-path "/home/sowmith/projects/datalog-examples/slog-examples/brouhaha/input/")
; going to have a map of relname to strings, each kv pair for each csv file
(define (process fact [rel_map (hash)])
  (match fact
    [`(store (f-addr [] ,fun-name) ,define-fact)
     (p-dbg (hash-set rel_map
                      'store
                      (cons `(,(symbol->string (gensym "")) ,fun-name ,(process define-fact rel_map))
                            (hash-ref rel_map 'store '()))))]
    [`(define ,fun-name
        ,params
        ,body)
     "yeehaw"]))

(define (write-to-file rel_name rel_map)
  (define file-path (string-append to-save-path (symbol->string rel_name) ".csv"))
  (define (lines-to-write list-of-facts)
    (string-join (map (lambda (fact) (string-join fact "\t")) list-of-facts) "\n"))
  (define file-out (open-output-file file-path #:exists 'replace))
  (fprintf file-out "~a" (lines-to-write (hash-ref rel_map rel_name '()))))

(define (write-relation rel_map)
  (foldl (lambda (rel acc) (write-to-file rel rel_map)) '() (hash-keys rel_map)))

(write-relation (process input))
