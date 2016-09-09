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

(define (form->qqchunk form-alist)
  "Collect all needed bits from the FORM-ALIST to build a <qqchunk>
representing the uploaded chunk."
  (define (as-string bv)
    (bytevector->string bv "ISO-8859-1"))
  (make-chunk
   (as-string (assoc-ref form-alist "qquuid"))
   (as-string (assoc-ref form-alist "qqfilename"))
   (assoc-ref form-alist "qqtotalfilesize")
   (assoc-ref form-alist "qqfile")))

(define (form-parts->alist parts)
  "Create an alist from the list of PARTS in which the form names map
to their respective values."
  (fold (lambda (part acc)
          (match (assoc-ref (part-headers part)
                            'content-disposition)
            ((path *** ('name . name))
             (cons `(,name . ,(part-body part))
                   acc))))
        '() parts))

(define %max-upload-size (* 100 1024 1024)) ;100 MiB

(define (upload-handler request body)
  "Handle the upload request and enqueue a job to process the file.
Returns a JSON status message that is understood by the Fine Uploader
JavaScript library."
  (let ((size (request-content-length request)))
    (if (> size %max-upload-size)
        (json
         (object
          ("error"
           ,(format #f "The file is too large!  File size must be less than ~a MiB."
                    (/ %max-upload-size 1024 1024)))))
        (let* ((parts (parse-request-body request body))
               (form-alist (form-parts->alist parts)))
          (catch #t
            (lambda ()
              (let ((result (save-uploaded-file form-alist)))
                ;; TODO: enqueue job!
                (json
                 (object
                  ("success" "true")
                  ("result" ,result)))))
            (lambda (key . rest)
              (json
               (object
                ("error" ,(with-output-to-string
                            (lambda ()
                              (format #t "~a: ~a" key rest))))))))))))

(define (save-uploaded-file form-alist)
  "Process the FORM-ALIST of the multipart request to write the
uploaded data to a local file.  Return the unique file id."
  ;; Currently, we don't support chunking, so there's only one chunk
  ;; containing the whole file.
  (let* ((chunk (form->qqchunk form-alist))
         (target-name (string-append rcas-web-upload-dir "/"
                                     (qqchunk-uuid chunk))))
    ;; Write the bytevector to file.  We don't care what's really
    ;; inside.
    (with-output-to-file target-name
      (lambda ()
        (put-bytevector (current-output-port)
                        (qqchunk-file chunk)))
      #:binary #t)
    (qqchunk-uuid chunk)))
