#lang racket

; Global variables to store temperatures, current post-processing function, and function table
(define temperatures '())
(define current-postproc #f)
(define function-table '())

; Validates if a temperature is a valid Fahrenheit value 
(define (valid-fahrenheit? temp)
  (and (number? temp)
       (>= temp -459.67)))

; Converts Celsius to Fahrenheit
(define (celsius-to-fahrenheit c)
  (+ (* c (/ 9 5)) 32))

; Converts Kelvin to Fahrenheit
(define (kelvin-to-fahrenheit k)
  (celsius-to-fahrenheit (- k 273.15)))

; Parses temperature input string and converts to Fahrenheit
(define (parse-temperature str)
  (let ([trimmed (string-trim str)])
    (cond
      [(string=? trimmed "") #f]
      [(regexp-match #rx"^[cC][ ]?(-?[0-9]+\\.?[0-9]*)$" trimmed)
       (let* ([match (regexp-match #rx"^[cC][ ]?(-?[0-9]+\\.?[0-9]*)$" trimmed)]
              [num (string->number (cadr match))])
         (and num 
              (let ([fahr (celsius-to-fahrenheit num)])
                (and (valid-fahrenheit? fahr) fahr))))]
      [(regexp-match #rx"^[fF][ ]?(-?[0-9]+\\.?[0-9]*)$" trimmed)
       (let* ([match (regexp-match #rx"^[fF][ ]?(-?[0-9]+\\.?[0-9]*)$" trimmed)]
              [num (string->number (cadr match))])
         (and num (valid-fahrenheit? num) num))]
      [(regexp-match #rx"^[kK][ ]?(-?[0-9]+\\.?[0-9]*)$" trimmed)
       (let* ([match (regexp-match #rx"^[kK][ ]?(-?[0-9]+\\.?[0-9]*)$" trimmed)]
              [num (string->number (cadr match))])
         (and num 
              (let ([fahr (kelvin-to-fahrenheit num)])
                (and (valid-fahrenheit? fahr) fahr))))]
      [(regexp-match #rx"^-?[0-9]+\\.?[0-9]*$" trimmed)
       (let ([num (string->number trimmed)])
         (and num (valid-fahrenheit? num) num))]
      [else #f])))

; Creates a post-processing function from a string expression
(define (create-postproc-function str)
  (let ([ns (make-base-namespace)])
    (with-handlers ([exn:fail? (λ (e) #f)])
      (eval `(lambda (x) ,(read (open-input-string str))) ns))))

; Processes post-processing command 
(define (process-postproc-command str)
  (let ([match (regexp-match #rx"^[pP][ ]?(.+)$" str)])
    (when match
      (let ([func (create-postproc-function (cadr match))])
        (when (procedure? func)
          (set! current-postproc (cons func (cadr match))))))))

; Stores current post-processing function in function table with given name
(define (store-function name)
  (when current-postproc
    (set! function-table 
          (cons (cons (string-trim name) current-postproc)
                (filter (λ (pair) (not (equal? (car pair) (string-trim name))))
                        function-table)))))

; Retrieves stored function by name and sets it as current post-processing function
(define (retrieve-function name)
  (let ([pair (assoc (string-trim name) function-table)])
    (when pair
      (set! current-postproc (cdr pair)))))

; Applies current post-processing function to temperature value
(define (apply-postproc temp)
  (if current-postproc
      (with-handlers ([exn:fail? (λ (e) temp)])
        (let ([result ((car current-postproc) temp)])
          (if (and (number? result) (valid-fahrenheit? result))
              result
              temp)))
      temp))

; Processes and stores valid temperature values
(define (process-temperature temp)
  (let ([processed-temp (apply-postproc temp)])
    (when processed-temp
      (set! temperatures (append temperatures (list processed-temp))))))

; Formats number to 2 decimal places to match output
(define (format-number n)
  (~r n #:precision '(= 2)))

; Displays stored temperature results and calculates average
(define (display-results)
  (if (null? temperatures)
      (printf "No samples stored~n")
      (begin
        (printf "Number of samples: ~a~n" (length temperatures))
        (for-each (λ (temp) (printf "~a~n" (format-number temp))) temperatures)
        (when (not (null? temperatures))
          (printf "Average value: ~a~n" 
                  (format-number (/ (apply + temperatures) (length temperatures))))))))

; Displays stored functions and their definitions
(define (display-functions)
  (if (null? function-table)
      (printf "No functions stored~n")
      (begin
        (printf "Number of functions: ~a~n" (length function-table))
        (for-each (λ (pair) 
                    (printf "~a (lambda (x) ~a)~n" 
                            (car pair)
                            (cdr (cdr pair))))
                  (sort function-table string<? #:key car)))))

; Flags for tracking display state
(define was-dl #f)
(define after-dc #f)

; Handles editing commands (dl, pl, dc, pc, pd)
(define (handle-edit-command str)
  (let ([cmd (string-trim str)])
    (cond
      [(string=? cmd "dl") 
       (display-results)
       (when (not (null? function-table))
         (printf "Number of functions: ~a~n" (length function-table))
         (for-each (λ (pair) 
                    (printf "~a (lambda (x) ~a)~n" 
                            (car pair)
                            (cdr (cdr pair))))
                  (sort function-table string<? #:key car)))]
      [(string=? cmd "pl") 
       (display-functions)]
      [(string=? cmd "dc") 
       (set! temperatures '())
       (display-results)
       (when (not (null? function-table))
         (printf "Number of functions: ~a~n" (length function-table))
         (for-each (λ (pair) 
                    (printf "~a (lambda (x) ~a)~n" 
                            (car pair)
                            (cdr (cdr pair))))
                  (sort function-table string<? #:key car)))]
      [(string=? cmd "pc") 
       (set! function-table '())
       (display-functions)]
      [(regexp-match #rx"^pd\\s+(.+)$" cmd)
       => (λ (match)
            (let ([name (string-trim (cadr match))])
              (set! function-table
                    (filter (λ (pair) 
                             (not (equal? (car pair) name)))
                           function-table))))])))

; Main program loop - processes input commands and temperatures
(define (main-loop)
  (let ([line (read-line)])
    (unless (eof-object? line)
      (let ([trimmed (string-trim line)])
        (cond
          [(regexp-match #rx"^[eE]" trimmed)
           (display-results)]
          [(regexp-match #rx"^[sS][ ]?(.+)$" trimmed)
           (let ([match (regexp-match #rx"^[sS][ ]?(.+)$" trimmed)])
             (store-function (cadr match)))
           (main-loop)]
          [(regexp-match #rx"^[rR][ ]?(.+)$" trimmed)
           (let ([match (regexp-match #rx"^[rR][ ]?(.+)$" trimmed)])
             (retrieve-function (cadr match)))
           (main-loop)]
          [(regexp-match #rx"^[iI]$" trimmed)
           (set! current-postproc #f)
           (main-loop)]
          [(regexp-match #rx"^[pP]" trimmed)
           (process-postproc-command trimmed)
           (main-loop)]
          [(regexp-match #rx"^(dl|dc|pl|pc|pd)" trimmed)
           (handle-edit-command trimmed)
           (main-loop)]
          [else
           (let ([temp (parse-temperature trimmed)])
             (when temp
               (process-temperature temp)))
           (main-loop)])))))

(main-loop)