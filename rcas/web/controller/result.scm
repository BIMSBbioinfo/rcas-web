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
  #:use-module (rcas utils jobs)
  #:use-module (rcas web view html)
  #:export (result-handler))

(define (result-handler id)
  (let ((status (get-status id))
        (result (get-result id)))
    (cond
     ((null? status)
      (invalid-result id))
     ((string-prefix? "waiting" status)
      (result-page id status #f #t))
     ((string-prefix? "processing" status)
      (result-page id status #f #t))
     ((string-prefix? "success" status)
      (result-page id status result #f))
     ((string-prefix? "failed" status)
      (result-page id status result #f)))))
