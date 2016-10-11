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

(define-module (rcas utils report)
  #:use-module (rcas config)
  #:use-module (rcas utils r)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 iconv)
  #:export (runReport
            sanitize-report-options
            genome->gtf-file
            report-form-options->options
            options-as-table
            msigdb-options))

(define valid-fields
  '(;;queryFilePath <-- always ignore!
    ;;outDir        <-- always ignore!
    ;;gffFilePath   <-- automatically assigned (dependent on genome)
    ;;printProcessedTables <-- always true
    (annotationSummary . boolean)
    (goAnalysis        . boolean)
    (msigdbAnalysis    . boolean)
    (motifAnalysis     . boolean)
    (genomeVersion     . string)
    (sampleN           . number)

    ;; We only store the msigdb key, which is resolved to a path at
    ;; job execution time.
    (msigdbFilePath    . string)))

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
             (message "loading RCAS...")
             (suppressMessages (library "RCAS"))
             (message "running RCAS...")
             (RCAS::runReport ,@options))))))

(define permitted-report-options (map car valid-fields))

(define (sanitize-report-options options-string)
  "Leave only whitelisted pairs in the given OPTIONS.  This procedure
operates on a serialized OPTIONS string from the key value store.  The
string is expected to be a valid S-expression."
  (let ((clean (filter (match-lambda
                         ((key . value)
                          (member key permitted-report-options)))
                       ;; Convert options to an S-expression
                       (call-with-input-string options-string read))))
    ;; Replace msigdb key with path to file.
    (let ((msigdb (assoc-ref options 'msigdbFilePath)))
      (if msigdb
          (cons `(msigdbFilePath . ,(msigdb->msigdb-file msigdb))
                (assoc-remove! clean 'msigdbFilePath))
          clean))))

(define (report-form-options->options options-alist)
  "Leave only whitelisted pairs in the given form OPTIONS-ALIST.
Convert matching pairs to an alist of Scheme values that can be passed
directly to the rcas-job."
  (let ((good-options
         (filter-map (match-lambda
                       ((key . value)
                        (let* ((newkey (and=> (form-name->r-name key)
                                              string->symbol))
                               (type   (assoc-ref valid-fields newkey)))
                          (and type
                               (let ((strval (bytevector->string value "ISO-8859-1")))
                                 (cons newkey
                                       (case type
                                         ((string)  strval)
                                         ((number)  (or (string->number strval) 0))
                                         ((boolean) (equal? strval "on"))
                                         (else      strval))))))))
                     options-alist)))

    ;; Ensure that boolean fields default to #f when they were not
    ;; selected.
    (fold (lambda (key options)
            (if (assoc key options)
                options
                (cons (cons key #f) options)))
          good-options
          '(annotationSummary goAnalysis msigdbAnalysis motifAnalysis))))

(define (genome->gtf-file genome)
  "Return path to GTF file for the given GENOME."
  (and=> (assoc-ref %config 'gtf-files)
         (cut assoc-ref <> (string->symbol genome))))

(define (msigdb->msigdb-file key)
  "Return path to msigdb file for the given KEY."
  (and=> (assoc-ref %config 'msigdb)
         (lambda (msigdb)
           (and=> (assoc-ref msigdb (string->symbol key)) cadr))))

(define (msigdb->label key)
  "Return label for the given msigdb KEY."
  (and=> (assoc-ref %config 'msigdb)
         (lambda (msigdb)
           (and=> (assoc-ref msigdb (string->symbol key)) car))))

(define (msigdb-options)
  "Extract labels and keys for all available msigdb files, if
available, and present them as a list of option tags."
  (and=> (assoc-ref %config 'msigdb)
         (lambda (msigdb)
           (map (match-lambda
                  ((key label file)
                   `(option (@ (value ,key))
                            ,label)))
                msigdb))))

(define (form-name->r-name name)
  "Convert option names from the HTML form to the option names as used
in R, i.e. to from hyphenated lowercase to camelCase."
  (if (string-null? name)
      #f
      (string-downcase (string-join (map (cut string-upcase <> 0 1)
                                         (string-split name #\-))
                                    "")
                       0 1)))

(define (prettify-options options)
  (map (match-lambda
         (('annotationSummary . value)
          (cons "Provide annotation summaries from overlap operations"
                (if value "yes" "no")))
         (('goAnalysis . value)
          (cons "Run GO term enrichment analysis"
                (if value "yes" "no")))
         (('msigdbAnalysis . value)
          (cons "Run gene set enrichment analysis"
                (if value "yes" "no")))
         (('motifAnalysis . value)
          (cons "Run motif analysis"
                (if value "yes" "no")))
         (('genomeVersion . genome)
          (cons "Genome version" genome))
         (('msigdbFilePath . msigdb)
          (cons "Gene set" (msigdb->label msigdb)))
         (('sampleN . value)
          (if (zero? value) #f
              (cons "Downsampled intervals"
                    (format #f "~:d" value)))))
       (call-with-input-string options read)))

(define (options-as-table options-string)
  `(table ,@(map (match-lambda
                   ((label . value)
                    `(tr (th ,label)
                         (td ,value)))
                   (_ ""))
                 (prettify-options options-string))))
