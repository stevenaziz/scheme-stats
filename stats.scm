; Authored by Steven Aziz
; Last Modified 10-27-2023

; Mean function
; Takes list of numbers and outputs the average
(define (mean values)
    (let ((n 0) (sum 0))
        (let read-next ((values values)) 
            (if (pair? values)
            (begin
                (set! n (+ n 1))
                (set! sum (+ sum (car values)))
                (read-next (cdr values))
            )))
        (/ sum n)
    )
)

; Standard Deviation function
; Takes list of numbers and outputs the standard deviation
(define (stddev values)
    (let ((mean (mean values)) (n 0) (sigma 0))
        (let read-next ((values values))
            (if (pair? values)
                (begin
                    (set! n (+ n 1))
                    (set! sigma (+ sigma (expt (- (car values) mean) 2)))
                    (read-next (cdr values))
                )
            )
        )
        (sqrt (/ sigma n))
    )
)

; Regression Alpha function
; Takes two lists of numbers and outputs the y-intercept of the linear regression
(define (regressiona xvalues yvalues)
    (let ((sigma_x 0) (sigma_y) (sigma_x2 0) (sigma_xy) (n 0))
        (let read-next ((xvalues xvalues) (yvalues yvalues))
            (if (boolean/and (pair? xvalues) (pair? yvalues))
                (begin
                    (set! n (+ n 1))
                    (set! sigma_x (+ sigma_x (car xvalues)))
                    (set! sigma_y (+ sigma_y (car yvalues)))
                    (set! sigma_xy (+ sigma_xy (* (car xvalues) (car yvalues))))
                    (set! sigma_x2 (+ sigma_x2 (expt (car xvalues) 2)))
                    (read-next (cdr xvalues) (cdr yvalues))
                )
            )
        )
        (/ (- (* sigma_y sigma_x2) (* sigma_x sigma_xy)) (- (* n sigma_x2) (expt sigma_x 2)))
    )
)

; Regression Beta function
; Takes two lists of numbers and outputs the slope of the linear regression
(define (regressionb xvalues yvalues)
    (let ((sigma_x 0) (sigma_y) (sigma_x2 0) (sigma_xy) (n 0))
        (let read-next ((xvalues xvalues) (yvalues yvalues))
            (if (boolean/and (pair? xvalues) (pair? yvalues))
                (begin
                    (set! n (+ n 1))
                    (set! sigma_x (+ sigma_x (car xvalues)))
                    (set! sigma_y (+ sigma_y (car yvalues)))
                    (set! sigma_xy (+ sigma_xy (* (car xvalues) (car yvalues))))
                    (set! sigma_x2 (+ sigma_x2 (expt (car xvalues) 2)))
                    (read-next (cdr xvalues) (cdr yvalues))
                )
            )
        )
        (/ (- (* n sigma_xy) (* sigma_x sigma_y)) (- (* n sigma_x2) (expt sigma_x 2)))
    )
)

; Pearson Correlation function
; Takes two lists of numbers and outputs number between -1 and 1 corresponding to the Pearson Correlation Coefficient
; Measure strength of relationship between xvalues and yvalues
(define (correlation xvalues yvalues)
    (let ((sigma_x 0) (sigma_y) (sigma_x2 0) (sigma_y2 0) (sigma_xy) (n 0))
        (let read-next ((xvalues xvalues) (yvalues yvalues))
            (if (boolean/and (pair? xvalues) (pair? yvalues))
                (begin
                    (set! n (+ n 1))
                    (set! sigma_x (+ sigma_x (car xvalues)))
                    (set! sigma_y (+ sigma_y (car yvalues)))
                    (set! sigma_xy (+ sigma_xy (* (car xvalues) (car yvalues))))
                    (set! sigma_x2 (+ sigma_x2 (expt (car xvalues) 2)))
                    (set! sigma_y2 (+ sigma_y2 (expt (car yvalues) 2)))
                    (read-next (cdr xvalues) (cdr yvalues))
                )
            )
        )
        (/ (- (* n sigma_xy) (* sigma_x sigma_y)) (sqrt (* (- (* n sigma_x2) (expt sigma_x 2)) (- (* n sigma_y2) (expt sigma_y 2)))))
    )
)

; Read-CSV function
; Takes filename of .csv (as string), boolean corresponding to header present, and column number
; Outputs list of all values in that column
(define (read-csv file header column)
    (let ((vals '()) (input-port (open-input-file file)))
        (if header
            (begin
                (read-string (char-set #\newline) input-port)
                (read-char input-port)
            )
        )
        (let next-char ((curr-str (string)) (curr-column 0) (input-port input-port))
            (if (eqv? column curr-column)
                (begin
                    (if (eof-object? (peek-char input-port))
                        (display "File read complete.")
                        (if (boolean/or (eqv? #\newline (peek-char input-port)) (eqv? #\, (peek-char input-port)))
                            (begin
                                (set! vals (cons (string->number curr-str) vals))
                                (read-string (char-set #\newline) input-port)
                                (read-char input-port)
                                (next-char (string) 0 input-port)
                            )
                            (begin
                                (set! curr-str (string-append curr-str (string (read-char input-port))))
                                (next-char curr-str curr-column input-port)
                            )
                        )
                    )
                )
                (begin
                    (read-string (char-set #\,) input-port)
                    (next-char curr-str (+ curr-column 1) input-port)
                )
            )
        )
        (close-input-port input-port)
        (set! vals (reverse! vals))
        vals
    )
)

; Extra Credit

(define (apply-regression sat gpa test) 0.0000000001)
