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
            get-status
            get-options
            get-result
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

(define (make-getter field)
  (lambda (file-name)
    (car (with-redis
          (get (string-append %prefix field ":" file-name))))))

(define get-status  (make-getter "status"))
(define get-options (make-getter "options"))
(define get-result  (make-getter "result"))

(define (set-status! file-name status)
  (set (string-append %prefix "status:" file-name)
       (string-append status ":"
                      (number->string (current-time)))))

(define (set-options! file-name options)
  (set (string-append %prefix "options:" file-name)
       (format #f "~a" options)))

(define (set-result! file-name result)
  (set (string-append %prefix "result:" file-name)
       (format #f "~a" result)))

(define (next-file)
  "Wait for a file name to appear on the waiting list, then move it to
the processing list and return the name."
  (basename
   (car (with-redis (brpoplpush %waiting %processing 0)))))

(define (enqueue raw-file-name options)
  "Append the basename of RAW-FILE-NAME to the waiting queue and store
the OPTIONS."
  (let ((file-name (basename raw-file-name)))
    (with-redis
     (transaction
      (rpush %waiting (list file-name))
      (set-status! file-name "waiting")
      (set-options! file-name options)))))

(define (done? file-name)
  "Return the processing result if the FILE-NAME has been processed or
#f if it has not."
  (let ((status (get-status file-name))
        (result (get-result file-name)))
    (string-prefix? "success" status)))

(define (process-next processor)
  "Get a file name from the waiting queue and run PROCESSOR on it.
This procedure blocks until processor exits.  Once PROCESSOR exits,
the job status and result are updated."
  (let* ((file-name (next-file))
         (options   (get-options file-name)))
    (with-redis
     (set-status! file-name "processing"))
    ;; Process the file!
    ;; TODO: adjust to return multiple values (message and success?)
    (let ((result (processor file-name options)))
      (with-redis
       (transaction
        (set-result! file-name result)
        ;; TODO: store error message in result if job failed
        (if (and result (string? result))
            (set-status! file-name "success")
            (set-status! file-name "failed"))
        ;; We're done processing this.
        (lrem %processing 0 file-name))))
    file-name))

(define (processing-num)
  "Return number of jobs in the processing queue."
  (car (with-redis (llen %processing))))

(define (waiting-num)
  "Return number of jobs waiting for processing."
  (car (with-redis (llen %waiting))))
