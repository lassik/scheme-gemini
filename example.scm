(import (scheme base) (gemini-client))

(define (for-each/between visit between list)
  (unless (null? list)
    (visit (car list))
    (for-each (lambda (x) (between) (visit x)) (cdr list))))

(define (main)
  (for-each/between
   (lambda (uri)
     (guard (err
             ((gemini-error? err)
              (display "*** Error: ")
              (display (gemini-response-code (gemini-error-response err)))
              (newline)))
       (let ((body
              (gemini-get
               uri
               (lambda (response)
                 (or (gemini-response-raise response)
                     (gemini-response-read-string-all response))))))
         (display body)
         (newline))))
   (lambda ()
     (newline)
     (display (make-string 80 #\-))
     (newline)
     (newline))
   '("gemini://gemini.circumlunar.space/capcom/"
     "gemini://gemini.circumlunar.space/docs/specification.gmi"
     "gemini://gemini.circumlunar.space/nonexistent")))

(main)
