(import (scheme base) (scheme process-context) (srfi 193)
        (gemini) (gemini client))

(define (display-gemini-error err)
  (let ((response (gemini-error-response err)))
    (write-string
     (string-append (gemini-response-meta response) "\n")
     (current-error-port))
    (exit (gemini-response-code response))))

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
