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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 iconv)
  #:export (sanitize-report-options
            report-form-options->options))

(define valid-fields
  '(;;queryFilePath <-- always ignore!
    ;;outDir        <-- always ignore!
    ;;(gffFilePath          . string)
    ;;(msigdbFilePath       . string)
    (annotationSummary    . boolean)
    (goAnalysis           . boolean)
    (msigdbAnalysis       . boolean)
    (motifAnalysis        . boolean)
    (genomeVersion        . string)
    (printProcessedTables . string)
    (sampleN              . number)))

(define permitted-report-options (map car valid-fields))

(define (sanitize-report-options options-string)
  "Leave only whitelisted pairs in the given OPTIONS.  This procedure
operates on a serialized OPTIONS string from the key value store.  The
string is expected to be a valid S-expression."
  (filter (match-lambda
            ((key . value)
             (member key permitted-report-options)))
          ;; Convert options to an S-expression
          (call-with-input-string options-string read)))

(define (report-form-options->options options-alist)
  "Leave only whitelisted pairs in the given form OPTIONS-ALIST.
Convert matching pairs to an alist of Scheme values that can be passed
directly to the rcas-job."
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
                                  ((number)  (string->number strval))
                                  ((boolean) (equal? strval "on"))
                                  (else      strval))))))))
              options-alist))

(define (form-name->r-name name)
  "Convert option names from the HTML form to the option names as used
in R, i.e. to from hyphenated lowercase to camelCase."
  (if (string-null? name)
      #f
      (string-downcase (string-join (map (cut string-upcase <> 0 1)
                                         (string-split name #\-))
                                    "")
                       0 1)))
