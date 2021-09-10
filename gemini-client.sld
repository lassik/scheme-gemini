(define-library (gemini-client)
  (export gemini-get
          gemini-error?
          gemini-error-response
          gemini-response?
          gemini-response-code
          gemini-response-first-digit
          gemini-response-second-digit
          gemini-response-success?
          gemini-response-redirect?
          gemini-response-meta
          gemini-response-port
          gemini-response-read-bytevector-all
          gemini-response-read-string-all
          gemini-response-raise)
  (import (scheme base))
  (cond-expand
    (chicken
     (import (chicken condition) (openssl) (uri-generic))))
  (cond-expand
    (chicken

     (define gemini-error?
       (condition-predicate 'gemini-error))

     (define gemini-error-response
       (condition-property-accessor 'gemini-error 'response #f))

     (define (make-gemini-error response)
       (make-property-condition 'gemini-error
                                'message "Gemini request failed"
                                'response response))))
  (begin

    (define-record-type gemini-respose
      (make-gemini-response code meta port)
      gemini-response?
      (code gemini-response-code)
      (meta gemini-response-meta)
      (port gemini-response-port))

    (define (gemini-response-first-digit response)
      (truncate-quotient (gemini-response-code response) 10))

    (define (gemini-response-second-digit response)
      (truncate-remainder (gemini-response-code response) 10))

    (define (gemini-response-success? response)
      (= 2 (gemini-response-first-digit response)))

    (define (gemini-response-redirect? response)
      (= 3 (gemini-response-first-digit response)))

    (define (gemini-response-raise response)
      (and (not (gemini-response-success? response))
           (raise (make-gemini-error response))))

    (define (gemini-response-read-bytevector-all response)
      (let ((port (gemini-response-port response)))
        (let loop ((whole (bytevector)))
          (let ((part (read-bytevector 10000 port)))
            (if (eof-object? part) whole
                (loop (bytevector-append whole part)))))))

    (define (gemini-response-read-string-all response)
      (utf8->string (gemini-response-read-bytevector-all response)))

    (define (malformed-first-line line)
      (error "Malformed first line" line))

    (define (read-cr-lf-terminated-line port)
      (let loop ((line ""))
        (let ((char (read-char port)))
          (if (eof-object? char)
              (malformed-first-line line)
              (if (char=? #\return char)
                  (let ((char (read-char port)))
                    (if (char=? #\newline char)
                        line
                        (malformed-first-line line)))
                  (loop (string-append line (string char))))))))

    (define (write-request to-server uri-string)
      (write-string (string-append uri-string "\r\n") to-server))

    (define (read-response from-server)
      (let ((line (read-cr-lf-terminated-line from-server)))
        (if (or (< (string-length line) 3)
                (not (char<=? #\0 (string-ref line 0) #\9))
                (not (char<=? #\0 (string-ref line 1) #\9))
                (not (char=? #\space (string-ref line 2))))
            (malformed-first-line line)
            (let ((code (string->number (string-copy line 0 2)))
                  (meta (string-copy line 3 (string-length line))))
              (make-gemini-response code meta from-server)))))

    (define (gemini-get uri handle-response)
      (let* ((uri-object (uri-reference uri))
             (uri-string (if (string? uri) uri (uri->string uri-object))))
        (unless (eq? 'gemini (uri-scheme uri-object))
          (error "Not a gemini URI" uri))
        (let-values (((from-server to-server)
                      (ssl-connect* hostname: (uri-host uri-object)
                                    port: (or (uri-port uri-object) 1965)
                                    verify?: #f)))
          (dynamic-wind (lambda () #f)
                        (lambda ()
                          (write-request to-server uri-string)
                          (handle-response (read-response from-server)))
                        (lambda ()
                          (close-input-port from-server)
                          (close-output-port to-server))))))))
