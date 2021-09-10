(import (scheme base) (srfi 193) (gemini-client))

(define (display-gemini-error err)
  (parameterize ((current-output-port (current-error-port)))
    (display "*** Error: ")
    (display (gemini-response-code (gemini-error-response err)))
    (newline)))

(define (main)
  (unless (= 1 (length (command-args)))
    (error "Usage"))
  (let ((uri (car (command-args))))
    (guard (err ((gemini-error? err) (display-gemini-error err)))
      (gemini-get
       uri
       (lambda (response)
         (gemini-response-raise response)
         (let loop ()
           (let ((port (gemini-response-port response)))
             (let ((bytes (read-bytevector 10000 port)))
               (unless (eof-object? bytes)
                 (write-bytevector bytes)
                 (loop))))))))))

(main)
