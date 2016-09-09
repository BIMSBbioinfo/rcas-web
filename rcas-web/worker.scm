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

(define-module (rcas-web worker)
  #:use-module (rcas-web config)
  #:use-module (rcas-web jobs)
  #:use-module (rcas-web r)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (worker-loop))

;; Snarfed from the sources of (guix build utils)
(define (mkdir-p dir)
  "Create directory DIR and all its ancestors."
  (define absolute?
    (string-prefix? "/" dir))

  (define not-slash
    (char-set-complement (char-set #\/)))

  (let loop ((components (string-tokenize dir not-slash))
             (root       (if absolute?
                             ""
                             ".")))
    (match components
      ((head tail ...)
       (let ((path (string-append root "/" head)))
         (catch 'system-error
           (lambda ()
             (mkdir path)
             (loop tail path))
           (lambda args
             (if (= EEXIST (system-error-errno args))
                 (loop tail path)
                 (apply throw args))))))
      (() #t))))

(define (runReport options)
  "Generate a RCAS report according to the provided OPTIONS.  All
output is redirected to log files."
  (let ((out (assoc-ref options 'outDir)))
    (mkdir-p out)
    (runR (scheme->R
           `((set! Rout (file (description . ,(string-append out "/Rout.log"))
                              (open . "wt")))
             (set! Rerr (file (description . ,(string-append out "/Rerr.log"))
                              (open . "wt")))
             (sink (file . Rout)
                   (type . "output"))
             (sink (file . Rerr)
                   (type . "message"))
             (library "RCAS")
             (RCAS::runReport ,@options))))))

(define (sanitize-report-options options)
  "Leave only whitelisted pairs in the given OPTIONS."
  (define permitted-report-options
    '(;;queryFilePath <-- always ignore!
      ;;outDir        <-- always ignore!
      gffFilePath
      msigdbFilePath
      annotationSummary
      goAnalysis
      msigdbAnalysis
      motifAnalysis
      genomeVersion
      printProcessedTables
      sampleN))
  (filter (match-lambda
            ((key . value)
             (member key permitted-report-options)))
          ;; Convert options to an S-expression
          (call-with-input-string options read)))

(define (rcas-job raw-file-name options)
  ;; Make sure the RAW-FILE-NAME exists, is readable, and is a regular
  ;; file.  Also make sure that it is in the upload directory and
  ;; nowhere else.  Don't use the RAW-FILE-NAME directly!
  (let* ((file-name (basename raw-file-name))
         (input     (string-append rcas-web-upload-dir "/"
                                   file-name))
         (outdir    (string-append rcas-web-results-dir "/"
                                   file-name)))
    (if (and (file-exists? input)
             (access? input R_OK))
        (begin
          (format #t "[~a] rcas-job started for ~a with ~a\n"
                  (current-time) file-name options)
          (runReport (fold cons (sanitize-report-options options)
                           `((queryFilePath . ,input)
                             (outDir        . ,outdir))))
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
