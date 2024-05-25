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

(define (update-relmap new-fact fact-type rel_map)
  (hash-set rel_map fact-type (cons new-fact (hash-ref rel_map fact-type '()))))

(define (process-list lst rel_map)
  (define list-size (length lst))
  (define fact-type (string->symbol (string-append "list-" (number->string list-size))))
  (define id (symbol->string (gensym "")))
  (define new-flat-fact (cons id lst))
  (define updated_relmap  (update-relmap new-flat-fact fact-type rel_map))
  `(,id ,updated_relmap))

; going to have a map of relname to list of strings, each kv pair for each csv file
(define (process fact [rel_map (hash)])
  (match fact
    [`(store (f-addr [] ,fun-name) ,define-fact)
     (define fact-type 'flat-store)
     (define id (symbol->string (gensym "")))
     (match-define (list define-fact-id new_rel_map) (process define-fact rel_map))
     (define new-flat-fact `(,id ,fun-name ,define-fact-id))
     (define updated_relmap (update-relmap new-flat-fact fact-type new_rel_map))
     `(,id ,updated_relmap)
     ]
    [`(define ,fun-name ,params ,body)
     (define fact-type 'flat-define)
     (define id (symbol->string (gensym "")))
     (match-define `(,params-type ,_) params)
     (match-define (list params-fact-id new_rel_map) (process params rel_map))
     (define new-flat-fact `(,id ,fun-name ,(symbol->string params-type) ,params-fact-id "body"))
     (define updated_relmap (update-relmap new-flat-fact fact-type new_rel_map))
     `(,id ,updated_relmap)]
    [`(fixedparam ,lst)
     (define fact-type 'flat-fixedparam)
     (define id (symbol->string (gensym "")))
     (match-define (list list-fact-id new_rel_map) (process-list lst rel_map))
     (define new-flat-fact `(,id ,list-fact-id))
     (define updated_relmap (update-relmap new-flat-fact fact-type new_rel_map))
     `(,id ,updated_relmap)
     ]
    ;; [`(varparam ,lst)
    ;;  "var-params"]
    ))

(define (write-to-file rel_name rel_map)
  (define file-path (string-append to-save-path (symbol->string rel_name) ".csv"))
  (define (lines-to-write list-of-facts)
    (string-join (map (lambda (fact) (string-join fact "\t")) list-of-facts) "\n"))
  (define file-out (open-output-file file-path #:exists 'replace))
  (fprintf file-out "~a" (lines-to-write (hash-ref rel_map rel_name '()))))

(define (write-relation rel_map)
  (foldl (lambda (rel acc) (write-to-file rel rel_map)) '() (hash-keys rel_map)))
(match-define (list id rel_map_final) (process input))
(displayln rel_map_final)
(write-relation rel_map_final)
