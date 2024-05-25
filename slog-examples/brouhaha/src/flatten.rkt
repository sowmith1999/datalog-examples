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

(define (relmap-update new-fact fact-type rel_map)
  (hash-set rel_map fact-type (cons new-fact (hash-ref rel_map fact-type '()))))

(define (process-list lst rel_map)
  (define list-size (length lst))
  (define fact-type (string->symbol (string-append "list-" (number->string list-size))))
  (define id (symbol->string (gensym "")))
  (define new-flat-fact (cons id lst))
  (define updated-relmap  (relmap-update new-flat-fact fact-type rel_map))
  `(,id ,updated-relmap))

; going to have a map of relname to list of strings, each kv pair for each csv file
(define (process fact [rel_map (hash)])
  (match fact
    [`(store (f-addr [] ,fun-name) ,define-fact)
     (define fact-type 'flat-store)
     (define id (symbol->string (gensym "")))
     (match-define (list define-fact-id new-rel-map) (process define-fact rel_map))
     (define new-flat-fact `(,id ,fun-name ,define-fact-id))
     (define updated-relmap (relmap-update new-flat-fact fact-type new-rel-map))
     `(,id ,updated-relmap)
     ]
    [`(define ,fun-name ,params ,body)
     (define fact-type 'flat-define)
     (define id (symbol->string (gensym "")))
     (match-define `(,params-type ,_) params)
     (match-define (list params-fact-id new-rel-map) (process params rel_map))
     (define new-flat-fact `(,id ,fun-name ,(symbol->string params-type) ,params-fact-id "body"))
     (define updated-relmap (relmap-update new-flat-fact fact-type new-rel-map))
     `(,id ,updated-relmap)]
    [`(fixedparam ,lst)
     (define fact-type 'flat-fixedparam)
     (define id (symbol->string (gensym "")))
     (match-define (list list-fact-id new-rel-map) (process-list lst rel_map))
     (define new-flat-fact `(,id ,list-fact-id))
     (define updated-relmap (relmap-update new-flat-fact fact-type new-rel-map))
     `(,id ,updated-relmap)
     ]
    ))

(define (write-to-file rel_name rel_map)
  (define file-path (string-append to-save-path (symbol->string rel_name) ".csv"))
  (define (lines-to-write list-of-facts)
    (string-join (map (lambda (fact) (string-join fact "\t")) list-of-facts) "\n"))
  (define file-out (open-output-file file-path #:exists 'replace))
  (fprintf file-out "~a" (lines-to-write (hash-ref rel_map rel_name '()))))

(define (write-relation rel_map)
  (foldl (lambda (rel acc) (write-to-file rel rel_map)) '() (hash-keys rel_map)))

;; (match-define (list id rel_map_final) (process input))
;; (displayln rel_map_final)
;; (write-relation rel_map_final)
;; There can be three things in the fact
;; 1) another nested fact
;; 2) some element
;; 3) A slog list - This can also have all the above

(define (process-list-fancy lst rel_map)
  (define list-size (length lst))
  (define fact-type (string->symbol (string-append "list-" (number->string list-size))))
  (define id (symbol->string (gensym "")))
  (define new-flat-fact (cons id lst))
  (define updated-relmap  (relmap-update new-flat-fact fact-type rel_map))
  `(,id ,fact-type ,updated-relmap))


(define (process-items items rel_map)
  (foldl
   (lambda (item acc)
     (match item
       [(? string? val)
        `(,(append (car acc) `(,val)) ,(cadr acc))]
       [`(,(? symbol? rel-name) ,items ...)
        (match-define (list id fact-type updated-relmap) (gen-process `(,rel-name ,@items) (cadr acc)))
        `(,(append (car acc) `(,fact-type ,id)) ,updated-relmap)]
       [`,(? list? slog-lst)
        #:when (andmap (lambda (val) (or (string? val) (number? val) (list? val))) slog-lst)
        (match-define (list id fact-type updated-relmap) (process-list-fancy slog-lst rel_map))
        `(,(append (car acc) `(,fact-type ,id)) ,updated-relmap)]
       ))
   `(() ,rel_map) items))

(define (gen-process fact [rel_map (hash)])
  (match fact
    [`(,(? symbol? rel-name) ,items ...)
     (define fact-type (string-append "flat-" (symbol->string rel-name)))
     (define id (symbol->string (gensym "")))
     (match-define (list new-flat-fact new-rel-map) (process-items items rel_map))
     (define updated-relmap (relmap-update new-flat-fact fact-type new-rel-map))
     `(,id ,fact-type ,updated-relmap)
     ]
    )
  )
(gen-process input)

