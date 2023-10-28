; Authored by Steven Aziz
; Last Modified 10-27-2023

; mean function
; takes list of numbers and outputs the average
(define (mean values)
    (let ((n 0) (sum 0))
        (let loop ((values values)) 
            (if (pair? values)
            (begin
                (set! n (+ n 1))
                (set! sum (+ sum (car values)))
                (loop (cdr values))
            )))
        (/ sum n)
    )
)

; standard deviation function
; takes list of numbers and outputs the standard deviation
(define (stddev values)
    (let ((mean (mean values)) (n 0) (sigma 0))
        (let loop ((values values))
            (if (pair? values)
                (begin
                    (set! n (+ n 1))
                    (set! sigma (+ sigma (expt (- (car values) mean) 2)))
                    (loop (cdr values))
                )
            )
        )
        (sqrt (/ sigma n))
    )
)

; regression a function
; takes two lists of numbers and outputs the y-intercept of the linear regression
(define (regressiona xvalues yvalues)
    (let ((sigma_x 0) (sigma_y) (sigma_x2 0) (sigma_xy) (n 0))
        (let loop ((xvalues xvalues) (yvalues yvalues))
            (if (eqv? (pair? xvalues) (pair? yvalues))
                (begin
                    (set! n (+ n 1))
                    (set! sigma_x (+ sigma_x (car xvalues)))
                    (set! sigma_y (+ sigma_y (car yvalues)))
                    (set! sigma_xy (+ sigma_xy (* (car xvalues) (car yvalues))))
                    (set! sigma_x2 (+ sigma_x2 (expt (car xvalues) 2)))
                    (loop (cdr xvalues) (cdr yvalues))
                )
            )
        )
        (/ (- (* sigma_y sigma_x2) (* sigma_x sigma_xy)) (- (* n sigma_x2) (expt sigma_x 2)))
    )
)

; regression b function
; takes two lists of numbers and outputs the slope of the linear regression
(define (regressionb xvalues yvalues)
    (let ((sigma_x 0) (sigma_y) (sigma_x2 0) (sigma_xy) (n 0))
        (let loop ((xvalues xvalues) (yvalues yvalues))
            (if (eqv? (pair? xvalues) (pair? yvalues))
                (begin
                    (set! n (+ n 1))
                    (set! sigma_x (+ sigma_x (car xvalues)))
                    (set! sigma_y (+ sigma_y (car yvalues)))
                    (set! sigma_xy (+ sigma_xy (* (car xvalues) (car yvalues))))
                    (set! sigma_x2 (+ sigma_x2 (expt (car xvalues) 2)))
                    (loop (cdr xvalues) (cdr yvalues))
                )
            )
        )
        (/ (- (* n sigma_xy) (* sigma_x sigma_y)) (- (* n sigma_x2) (expt sigma_x 2)))
    )
)

; correlation function
; takes two lists of numbers and outputs number between -1 and 1 corresponding to the Pearson Correlation Coefficient
; measure strength of relationship between xvalues and yvalues
(define (correlation xvalues yvalues)
    (let ((sigma_x 0) (sigma_y) (sigma_x2 0) (sigma_y2 0) (sigma_xy) (n 0))
        (let loop ((xvalues xvalues) (yvalues yvalues))
            (if (eqv? (pair? xvalues) (pair? yvalues))
                (begin
                    (set! n (+ n 1))
                    (set! sigma_x (+ sigma_x (car xvalues)))
                    (set! sigma_y (+ sigma_y (car yvalues)))
                    (set! sigma_xy (+ sigma_xy (* (car xvalues) (car yvalues))))
                    (set! sigma_x2 (+ sigma_x2 (expt (car xvalues) 2)))
                    (set! sigma_y2 (+ sigma_y2 (expt (car yvalues) 2)))
                    (loop (cdr xvalues) (cdr yvalues))
                )
            )
        )
        (/ (- (* n sigma_xy) (* sigma_x sigma_y)) (sqrt (* (- (* n sigma_x2) (expt sigma_x 2)) (- (* n sigma_y2) (expt sigma_y 2)))))
    )
)

; read-csv function
; takes filename of .csv (as string), boolean corresponding to header present, and column number
; outputs list of all values in that column
(define (read-csv file header column)
    (let ((vals (list)))
    (open-input-file file)
    )
    ;vals
)

; Extra Credit placeholder

(define (apply-regression sat gpa test)
    0.0000000001)