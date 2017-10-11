;;; rcas-web - Web interface for RCAS
;;; Copyright © 2016, 2017  Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2014  David Thompson <davet@gnu.org>
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

(define-module (rcas web server)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (web http)
  #:use-module (web request)
  #:use-module (web server)
  #:use-module (web uri)
  #:use-module (rcas web controller)
  #:use-module (rcas web render)
  #:use-module (rcas web util)
  #:export (start-rcas-web))

(define (run-controller controller request body)
  ((controller request body)
   (cons (request-method request)
         (request-path-components request))))

(define (handler request body controller)
  (format #t "~a ~a\n"
          (request-method request)
          (uri-path (request-uri request)))
  (apply values
         (append
          (run-controller controller request body)
          (list controller))))

(define (start-rcas-web port)
  (run-server (lambda args (apply handler args))
              'http
              `(#:addr ,INADDR_ANY
                #:port ,port)
              controller))
