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

(define-module (rcas web controller result)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:use-module (rcas utils jobs)
  #:use-module (rcas web view html)
  #:export (result-handler))

(define (split-status status)
  (match-let* (((status time) (string-split status #\:))
               (secs (- (current-time) (string->number time)))
               (ago (cond
                     ((< secs 60)
                      (format #f "~a second~:p" secs))
                     ((< secs (* 60 60))
                      (format #f "~a minute~:p"
                              (quotient secs 60)))
                     ((< secs (* 60 60 24))
                      (format #f "~a hour~:p"
                              (quotient secs (* 60 60))))
                     (else
                      (format #f "~a day~:p"
                              (quotient secs (* 60 60 24)))))))
    (values status ago)))

(define (result-handler id)
  (let ((status (get-status id))
        (result (get-result id)))
    (cond
     ((null? status)
      (invalid-result id))
     (else
      (let-values (((status ago) (split-status status)))
        (case (string->symbol status)
          ((waiting)
           (result-page id status ago #f #t))
          ((processing)
           (result-page id status ago #f #t))
          ((success)
           (result-page id status ago result #f))
          ((failed)
           (result-page id status ago result #f))))))))
