#lang racket

; yee another empty file
(require print-debug/print-dbg)
(define to-save-path "/home/sowmith/projects/datalog-examples/slog-examples/brouhaha/input/")

(define test-input-list
  '((app (ref "=") [(int "0")(ref "n") "1"])))
(define input-list
  '((store (f-addr [] "list-ref") (define "list-ref" (fixedparam ["lst" "n" ]) (if (app (ref "=") [(int "0")(ref "n")]) (app (ref "car") [(ref "lst")]) (app (ref "list-ref") [(app (ref "cdr") [(ref "lst")])(app (ref "-") [(ref "n")(int "1")])]))))
    (store (f-addr [] "member") (define "member" (fixedparam ["item" "lst" ]) (if (if (app (ref "null?") [(ref "item")]) (app (ref "null?") [(ref "item")]) (app (ref "null?") [(ref "lst")])) (bool "f") (if (app (ref "equal?") [(ref "item")(app (ref "car") [(ref "lst")])]) (ref "lst") (app (ref "member") [(ref "item")(app (ref "cdr") [(ref "lst")])])))))
    ))

;; There can be three things in the fact
;; 1) another nested fact
;; 2) some element
;; 3) A slog list - This can also have all the above

(define (relmap-update new-fact fact-type rel_map)
  (hash-set rel_map fact-type (cons new-fact (hash-ref rel_map fact-type '()))))

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
        (match-define (list id fact-type updated-relmap) (gen-process `("slog-lst" ,slog-lst) (cadr acc)))
        `(,(append (car acc) `(,fact-type ,id)) ,updated-relmap)]
       ))
   `(() ,rel_map) items))

(define (gen-process fact [rel_map (hash)])
  (match fact
    [`(,(? symbol? rel-name) ,items ...)
     (define fact-type (string-append "flat-" (symbol->string rel-name)))
     (define id (gensym ""))
     (match-define (list new-flat-fact new-rel-map) (process-items items rel_map))
     (define updated-relmap (relmap-update `(,id ,@new-flat-fact) fact-type new-rel-map))
     `(,id ,fact-type ,updated-relmap)
     ]
    [`("slog-lst" ,items)
     (define fact-type (string-append "flat-list-" (number->string (length items))))
     (define id (gensym ""))
     (match-define (list new-flat-fact new-rel-map) (process-items items rel_map))
     (define updated-relmap (relmap-update `(,id ,@new-flat-fact) fact-type new-rel-map))
     `(,id ,fact-type ,updated-relmap)]
    )
  )

(define (write-to-file rel_name rel_map)
  (define file-path (string-append to-save-path rel_name ".csv"))
  (define (str-correct item)
    (if (string? item) (string-append "\"" item "\"") (symbol->string item)))
  (define (lines-to-write list-of-facts)
    (string-join (map (lambda (fact) (string-join (map str-correct fact) "\t")) list-of-facts) "\n"))
  (define file-out (open-output-file file-path #:exists 'replace))
  (fprintf file-out "~a" (lines-to-write (hash-ref rel_map rel_name '()))))

(define (write-relation rel_map)
  (foldl (lambda (rel acc) (write-to-file rel rel_map)) '() (hash-keys rel_map)))

(define (process-list-of-facts input-list [rel_map (hash)])
  (foldl (lambda (fact acc) (caddr (gen-process fact acc)))
         rel_map input-list))

(define rel_map (process-list-of-facts test-input-list))
(pretty-print rel_map)
;; (write-relation rel_map)

;; [(flat-list-3 id "flat-ref" flat-ref1-id "flat-ref" flat-ref2-id "flat-app" flat-app1-id)
;;  (ref-holder flat-ref1-id lifted-ref1-fact)
;;  (ref-holder flat-ref2-id lifted-ref2-fact)
;;  (app-holder flat-app1-id lifted-app1-fact)
;;    --> (list-holder id [lifted-ref1-fact lifted-ref2-fact lifted-app1-fact])]

(define (make-main-body rel_name fact arity [head (list `(,(string->symbol rel_name)) `())] [body (list 'list-holder)])
  (if (= arity 0)
      `(,head ,body)
      (if (= 1 (length (car head)))
          (make-main-body rel_name (cdr fact) arity
                          (p-dbg (list (append (car head) `(id)) `())) (append body `(id)))
          (let*
              ((rel_string (car fact))
               (rel_id_name (string->symbol (string-append rel_string (number->string arity) (if (eq? rel_string "datum") "-val" "-id"))))
               (new_head (p-dbg (list (append (car head) `(,rel_string ,rel_id_name)) (cadr head))))
               (lifted_fact_name (if (eq? "datum" rel_string) rel_id_name (string->symbol (string-append "lifted-" rel_string (number->string arity) "-fact"))))
               (new_body (append body `(,lifted_fact_name)))
               (updated-new-head (p-dbg (if (eq? "datum" rel_string) new_head (list (car new_head) (append (cadr new_head) (list (list (string->symbol (string-append rel_string "-holder")) rel_id_name lifted_fact_name))))))))
            
            (make-main-body rel_name (cdr (cdr fact)) (- arity 1) updated-new-head new_body)
            )
          )
      )
  )


(define (lifting-rules-for-list fact-list rel_name rule_map)
  (define arity (/ (- (length (car fact-list)) 1) 2))
  (make-main-body rel_name (car fact-list) arity))


(pretty-print (lifting-rules-for-list (hash-ref rel_map "flat-list-3") "flat-list-3" (hash)))

