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
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (rcas config)
  #:use-module (rcas utils jobs)
  #:use-module (rcas utils report)
  #:use-module (rcas web view html)
  #:export (result-handler
            pack-and-send-report))

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

(define (log-for id type)
  (let* ((out  (string-append (assoc-ref %config 'results-dir) "/" id))
         (file (string-append out "/R" (symbol->string type) ".log"))
         (buffer-size 2048))
    (if (file-exists? file)
        ;; get the last BUFFER-SIZE bytes
        (with-input-from-file file
          (lambda ()
            (seek (current-input-port) 0 SEEK_END)
            (let ((maxoffset (min (ftell (current-input-port))
                                  buffer-size)))
              (seek (current-input-port) (- maxoffset) SEEK_END)
              (read-string (current-input-port) maxoffset))))
        #f)))

(define (result-handler id)
  (let ((status (get-status id)) )
    (cond
     ((null? status)
      (invalid-result id))
     (else
      (let-values (((status ago) (split-status status))
                   (errors       (log-for id 'err))
                   (output       (log-for id 'out))
                   (result       (get-result id))
                   (options      (options-as-table (get-options id))))
        (case (string->symbol status)
          ((waiting)
           (result-page id status ago options errors output #f #t))
          ((processing)
           (result-page id status ago options errors output #f #t
                        ;; FIXME: parse log to find the progress of
                        ;; report generation.  This is really a hack
                        ;; and it might fail, because we cannot
                        ;; guarantee that the little snippet we have
                        ;; from the log contains a match.
                        (or (and (car output)
                                 (and=> (string-match ".*\\| +([0-9]+)%"
                                                      (car output))
                                        (cut match:substring <> 1)))
                            "0")))
          ((success)
           (result-page id status ago options errors output result #f))
          ((failed)
           (result-page id status ago options errors output result #f))))))))

(define (pack-and-send-report id)
  "Tar up the report directory for the report with the given ID and
send it."
  (let ((status (get-status id)))
    (cond
     ((null? status) #f)
     (else
      (let* ((dir (string-append (assoc-ref %config 'results-dir) "/" id))
             (archive (string-append (or (getenv "TMPDIR") "/tmp")
                                     "/" id ".tar.gz")))
        (if (and (file-exists? dir)
                 (zero? (system* "tar" "-C" dir "-czf" archive ".")))
            (let ((response
                   (list
                    `((content-type . (application/gzip))
                      (content-disposition . (attachment
                                              (filename . ,(basename archive)))))
                    (call-with-input-file archive get-bytevector-all))))
              (delete-file archive)
              response)
            #f))))))
