;;; rcas-web - Web interface for RCAS
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (rcas ui)
  #:use-module (ice-9 match)
  #:use-module (rcas web server)
  #:use-module (rcas web controller)
  #:use-module (rcas utils worker)
  #:export (run-rcas-web))

(define (show-rcas-web-usage)
  (format (current-error-port)
          "
    `rcas-web server': start the application web server.
    `rcas-web worker': start a worker process (requires Redis).
")
  (exit 1))

(define (run-rcas-web . args)
  (match args
    (()
     (format (current-error-port)
             "rcas-web: missing command name")
     (show-rcas-web-usage))
    ((or ("-h") ("--help") ("help"))
     (show-rcas-web-usage))
    (("worker")
     (worker-loop))
    (("server")
     (start-rcas-web controller))
    (_  (format (current-error-port)
                "rcas-web: unknown command ~a\n" args))))
