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

(define-module (rcas-web controller upload)
  #:use-module (ice-9 match)
  #:use-module (ice-9 iconv)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (json)
  #:use-module (rnrs io ports)
  #:use-module (web request)
  #:use-module (rcas-web multipart)
  #:use-module (rcas-web config)
  #:export (upload-handler))

(define-record-type <qqchunk>
  (make-chunk uuid file-name total-file-size file)
  qqchunk?
  (uuid             qqchunk-uuid)
  (file-name        qqchunk-file-name)
  (total-file-size  qqchunk-total-file-size)
  (file             qqchunk-file))

(define (parts-to-chunk parts)
  "Collect all needed bits from the list of PARTS to build a <qqchunk>
representing the uploaded chunk."
  (define (as-string bv)
    (bytevector->string bv "ISO-8859-1"))
  (let ((chunk-alist (fold (lambda (part acc)
                             (match (assoc-ref (part-headers part)
                                               'content-disposition)
                               ((path *** ('name . name))
                                (cons `(,name . ,(part-body part))
                                      acc))))
                           '() parts)))
    (make-chunk
     (as-string (assoc-ref chunk-alist "qquuid"))
     (as-string (assoc-ref chunk-alist "qqfilename"))
     (assoc-ref chunk-alist "qqtotalfilesize")
     (assoc-ref chunk-alist "qqfile"))))

(define %max-upload-size (* 100 1024 1024)) ;100 MiB

(define (upload-handler request body)
  (let ((size (request-content-length request)))
    (if (> size %max-upload-size)
        (json
         (object
          ("error"
           ,(format #f "The file is too large!  File size must be less than ~a MiB."
                    (/ %max-upload-size 1024 1024)))))
        (let ((parts (parse-request-body request body)))
          (save-uploaded-file parts)))))

(define (save-uploaded-file parts)
  "Process the PARTS of the multipart request to write the uploaded
data to a local file.  Returns a JSON status message that is
understood by the Fine Uploader JavaScript library."
  ;; Currently, we don't support chunking, so there's only one chunk
  ;; containing the whole file.
  ;; TODO: sanitize original file name?
  (let* ((chunk (parts-to-chunk parts))
         (target-name (string-append rcas-web-upload-dir "/"
                                     (qqchunk-uuid chunk) "-"
                                     (qqchunk-file-name chunk))))
    (catch #t
      (lambda ()
        ;; Write the bytevector to file.  We don't care what's really
        ;; inside.
        (with-output-to-file target-name
          (lambda ()
            (put-bytevector (current-output-port)
                            (qqchunk-file chunk)))
          #:binary #t)
        (json
         (object
          ("success" "true"))))
      (lambda (key . rest)
        (json
         (object
          ("error" ,(with-output-to-string
                      (lambda ()
                        (format #t "~a: ~a" key rest))))))))))
