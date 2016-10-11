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

(define-module (rcas utils worker)
  #:use-module (rcas config)
  #:use-module (rcas utils report)
  #:use-module (rcas utils jobs)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (worker-loop))

(define (rcas-job raw-file-name options)
  ;; Make sure the RAW-FILE-NAME exists, is readable, and is a regular
  ;; file.  Also make sure that it is in the upload directory and
  ;; nowhere else.  Don't use the RAW-FILE-NAME directly!
  (let* ((file-name (basename raw-file-name))
         (input     (string-append (assoc-ref %config 'upload-dir) "/"
                                   file-name))
         (outdir    (string-append (assoc-ref %config 'results-dir) "/"
                                   file-name)))
    (if (and (file-exists? input)
             (access? input R_OK))
        (begin
          (let ((options (sanitize-report-options options)))
            (format #t "[~a] rcas-job started for ~a with ~a\n"
                    (current-time) file-name options)
            (runReport (fold cons options
                             `((selfContained . #f)
                               (queryFilePath . ,input)
                               (outDir        . ,outdir)
                               (gffFilePath   . ,(genome->gtf-file
                                                  (assoc-ref options 'genomeVersion)))))))
          (let ((result-file (string-append file-name ".RCAS.report.html")))
            (if (file-exists? (string-append outdir "/" result-file))
                result-file
                #f)))
        (begin
          (format #t "[~a] ERROR rcas-job: cannot access file ~a\n"
                  (current-time) file-name)
          #f))))

(define (worker-loop)
  "Process jobs forever.  Blocks if there are no jobs."
  (let ((processed (process-next rcas-job)))
    (format #t "[~a] done: ~a\n"
            (current-time) processed))
  (worker-loop))
