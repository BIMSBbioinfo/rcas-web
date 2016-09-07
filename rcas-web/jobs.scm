;;; rcas-web - Web interface for RCAS
;;; Copyright © 2016  Ricardo Wurmus <rekado@elephly.net>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (rcas-web jobs)
  #:use-module (redis)
  #:export (enqueue
            done?
            process-next
            processing-num
            waiting-num))

(define (flatten lst)
  (cond ((null? lst) '())
        ((pair? lst)
         ((@@ (guile) append) (flatten (car lst))
                 (flatten (cdr lst))))
        (else (list lst))))

;; Take care of opening and closing a connection to redis.
(define-syntax with-redis
  (lambda (x)
    (syntax-case x ()
      ((_ exp ...)
       (with-syntax (($conn (datum->syntax x '$conn)))
         #'(let (($conn (redis-connect)))
             (dynamic-wind
               (lambda () #t)
               (lambda () (redis-send $conn
                                      (flatten (list exp ...))))
               (lambda () (redis-close $conn)))))))))

(define-syntax-rule (transaction exps ...)
  (list (multi)
        exps ...
        (exec)))

(define %prefix     "rcas-web:")
(define %waiting    (string-append %prefix "waiting"))
(define %processing (string-append %prefix "processing"))

(define (enqueue filename options)
  "Append FILENAME to the waiting queue and store the OPTIONS."
  (with-redis
   (transaction
    (rpush %waiting (list filename))
    (set (string-append %prefix ":options:" filename)
         (format #f "~a" options)))))

(define (done? filename)
  "Return the processing result if the FILENAME has been processed or
#f if it has not."
  (let ((result
         (car (with-redis (get (string-append %prefix filename))))))
    (if (null? result) #f
        result)))

(define (process-next processor)
  "Get a file name from the waiting queue and run PROCESSOR on it.
This procedure blocks until processor exits.  Once PROCESSOR exits, an
entry is created with the processed file name as key and the result as
the value."
  ;; Block until there's something to process.
  (let* ((filename (car (with-redis
                         (brpoplpush %waiting %processing 0))))
         (options  (car (with-redis
                         (get (string-append %prefix ":options:" filename)))))
         ;; Process the file!
         (result   (processor filename options)))
    (with-redis
     (transaction
      ;; Save the result.
      (set (string-append %prefix filename)
           (string-append result ":"
                          (number->string (current-time))))
      ;; We're done processing this.
      (lrem %processing 0 filename)))
    filename))

(define (processing-num)
  "Return number of jobs in the processing queue."
  (car (with-redis (llen %processing))))

(define (waiting-num)
  "Return number of jobs waiting for processing."
  (car (with-redis (llen %waiting))))
