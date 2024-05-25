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
(define input-list
  '((store (f-addr [] "list-ref") (define "list-ref" (fixedparam ["lst" "n" ]) (if (app (ref "=") [(int "0")(ref "n")]) (app (ref "car") [(ref "lst")]) (app (ref "list-ref") [(app (ref "cdr") [(ref "lst")])(app (ref "-") [(ref "n")(int "1")])]))))
    (store (f-addr [] "member") (define "member" (fixedparam ["item" "lst" ]) (if (if (app (ref "null?") [(ref "item")]) (app (ref "null?") [(ref "item")]) (app (ref "null?") [(ref "lst")])) (bool "f") (if (app (ref "equal?") [(ref "item")(app (ref "car") [(ref "lst")])]) (ref "lst") (app (ref "member") [(ref "item")(app (ref "cdr") [(ref "lst")])])))))
    (store (f-addr [] "map") (define "map" (fixedparam ["proc" "lst" ]) (if (app (ref "null?") [(ref "lst")]) (app (ref "list") []) (app (ref "cons") [(app (ref "proc") [(app (ref "car") [(ref "lst")])])(app (ref "map") [(ref "proc")(app (ref "cdr") [(ref "lst")])])]))))
    (store (f-addr [] "filter") (define "filter" (fixedparam ["op" "lst" ]) (if (app (ref "null?") [(ref "lst")]) (app (ref "list") []) (if (app (ref "op") [(app (ref "car") [(ref "lst")])]) (app (ref "cons") [(app (ref "car") [(ref "lst")])(app (ref "filter") [(ref "op")(app (ref "cdr") [(ref "lst")])])]) (app (ref "filter") [(ref "op")(app (ref "cdr") [(ref "lst")])])))))
    (store (f-addr [] "foldr") (define "foldr" (fixedparam ["fun" "acc" "lst" ]) (if (app (ref "null?") [(ref "lst")]) (ref "acc") (app (ref "fun") [(app (ref "car") [(ref "lst")])(app (ref "foldr") [(ref "fun")(ref "acc")(app (ref "cdr") [(ref "lst")])])]))))
    (store (f-addr [] "append1") (define "append1" (fixedparam ["lhs" "rhs" ]) (if (app (ref "null?") [(ref "lhs")]) (ref "rhs") (app (ref "cons") [(app (ref "car") [(ref "lhs")])(app (ref "append1") [(app (ref "cdr") [(ref "lhs")])(ref "rhs")])]))))
    (store (f-addr [] "append") (define "append" (varparam "vargs") (let [ (binding "l1" (app (ref "car") [(ref "vargs")])) (binding "vargs8779" (app (ref "cdr") [(ref "vargs")]))] (let [ (binding "l2" (app (ref "car") [(ref "vargs8779")])) (binding "vargs8780" (app (ref "cdr") [(ref "vargs8779")]))] (let [ (binding "lsts" (ref "vargs8780"))] (if (app (ref "null?") [(ref "lsts")]) (app (ref "append1") [(ref "l1")(ref "l2")]) (app (ref "foldr") [(ref "append1")(app (ref "list") [])(app (ref "append1") [(app (ref "list") [(ref "l1")(ref "l2")])(ref "lsts")])])))))))
    (store (f-addr [] "list-set") (define "list-set" (fixedparam ["lst" "index" "value" ]) (if (app (ref "=") [(ref "index")(int "0")]) (app (ref "cons") [(ref "value")(app (ref "cdr") [(ref "lst")])]) (app (ref "cons") [(app (ref "car") [(ref "lst")])(app (ref "list-set") [(app (ref "cdr") [(ref "lst")])(app (ref "-") [(ref "index")(int "1")])(ref "value")])]))))
    (store (f-addr [] "flatten") (define "flatten" (fixedparam ["lst" ]) (if (app (ref "null?") [(ref "lst")]) (app (ref "list") []) (if (app (ref "pair?") [(ref "lst")]) (app (ref "append") [(app (ref "flatten") [(app (ref "car") [(ref "lst")])])(app (ref "flatten") [(app (ref "cdr") [(ref "lst")])])]) (app (ref "list") [(ref "lst")])))))
    (store (f-addr [] "ack") (define "ack" (fixedparam ["m" "n" ]) (if (app (ref "=") [(ref "m")(int "0")]) (app (ref "+") [(ref "n")(int "1")]) (if (app (ref "=") [(ref "n")(int "0")]) (app (ref "ack") [(app (ref "-") [(ref "m")(int "1")])(int "1")]) (app (ref "ack") [(app (ref "-") [(ref "m")(int "1")])(app (ref "ack") [(ref "m")(app (ref "-") [(ref "n")(int "1")])])])))))
    (store (f-addr [] "brouhaha_main") (define "brouhaha_main" (fixedparam []) (app (ref "ack") [(int "3")(int "12")]))))
  )

(define to-save-path "/home/sowmith/projects/datalog-examples/slog-examples/brouhaha/input/")

(define (relmap-update new-fact fact-type rel_map)
  (hash-set rel_map fact-type (cons new-fact (hash-ref rel_map fact-type '()))))

;; There can be three things in the fact
;; 1) another nested fact
;; 2) some element
;; 3) A slog list - This can also have all the above


(define (process-items items rel_map)
  (foldl
   (lambda (item acc)
     (match item
       [(? string? val)
        `(,(append (car acc) `("datum" ,val)) ,(cadr acc))]
       [`(,(? symbol? rel-name) ,items ...)
        (match-define (list id fact-type updated-relmap) (gen-process `(,rel-name ,@items) (cadr acc)))
        `(,(append (car acc) `(,fact-type ,id)) ,updated-relmap)]
       [`,(? list? slog-lst)
        #:when (andmap (lambda (val) (or (string? val) (number? val) (list? val))) slog-lst)
        (match-define (list id fact-type updated-relmap) (gen-process `("slog-lst" ,slog-lst) rel_map))
        `(,(append (car acc) `(,fact-type ,id)) ,updated-relmap)]
       ))
   `(() ,rel_map) items))

(define (gen-process fact [rel_map (hash)])
  (match fact
    [`(,(? symbol? rel-name) ,items ...)
     (define fact-type (string-append "flat-" (symbol->string rel-name)))
     (define id (symbol->string (gensym "")))
     (match-define (list new-flat-fact new-rel-map) (process-items items rel_map))
     (define updated-relmap (relmap-update `(,id ,@new-flat-fact) fact-type new-rel-map))
     `(,id ,fact-type ,updated-relmap)
     ]
    [`("slog-lst" ,items)
     (define fact-type (string-append "list-" (number->string (length items))))
     (define id (symbol->string (gensym "")))
     (match-define (list new-flat-fact new-rel-map) (process-items items rel_map))
     (define updated-relmap (relmap-update `(,id ,@new-flat-fact) fact-type new-rel-map))
     `(,id ,fact-type ,updated-relmap)]
    )
  )

(define (write-to-file rel_name rel_map)
  (define file-path (string-append to-save-path rel_name ".csv"))
  (define (lines-to-write list-of-facts)
    (string-join (map (lambda (fact) (string-join fact "\t")) list-of-facts) "\n"))
  (define file-out (open-output-file file-path #:exists 'replace))
  (fprintf file-out "~a" (lines-to-write (hash-ref rel_map rel_name '()))))

(define (write-relation rel_map)
  (foldl (lambda (rel acc) (write-to-file rel rel_map)) '() (hash-keys rel_map)))

(define (process-list-of-facts input-list [rel_map (hash)])
  (foldl (lambda (fact acc) (caddr (gen-process fact acc)))
         rel_map input-list))

(define rel_map (process-list-of-facts input-list))
(pretty-print rel_map)
(write-relation rel_map)

