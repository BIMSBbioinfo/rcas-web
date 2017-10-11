;;; rcas-web - Web interface for RCAS
;;; Copyright © 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (srfi srfi-1)
  #:use-module (rcas config)
  #:use-module (rcas web server)
  #:use-module (rcas utils worker)
  #:export (run-rcas-web))

(define (show-rcas-web-usage)
  (format (current-error-port)
          "
    `rcas-web [--config=rcas.conf] server [port]':
         start the application web server on port PORT or 8080 if omitted.

    `rcas-web [--config=rcas.conf] worker':
         start a worker process (requires Redis).
")
  (exit 1))

(define (maybe-override-config! args)
  (let ((config-option (find (lambda (opt)
                               (string-prefix? "--config=" opt))
                             args)))
    (if config-option
        (let ((file (cadr (string-split config-option #\=))))
          (when (file-exists? file)
            (let ((newconfig (call-with-input-file file read)))
              (set! %config (append newconfig %config))
              (delete config-option args))))
        args)))

(define (run-rcas-web . args)
  (let ((args (maybe-override-config! args)))
    (match args
        (()
         (format (current-error-port)
                 "rcas-web: missing command name\n")
         (show-rcas-web-usage))
      ((or ("-h") ("--help") ("help"))
       (show-rcas-web-usage))
      (("worker")
       (validate-configuration %config)
       (worker-loop))
      (("server" port ...)
       (validate-configuration %config)
       (let ((port (match port
                     (()  (assoc-ref %config 'port))
                     ((p) (string->number p)))))
         (format #t "Starting web server on port ~a\nCtrl-C to quit\n\n" port)
         (start-rcas-web port)))
      (_  (format (current-error-port)
                  "rcas-web: unknown command ~a\n" args)))))
