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

(define-module (rcas web multipart)
  #:use-module (rcas web bytevectors)
  #:use-module (ice-9 match)
  #:use-module (ice-9 iconv)
  #:use-module ((ice-9 binary-ports) #:select (unget-bytevector))
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module ((web http) #:select (read-headers))
  #:use-module (web request)
  #:export (parse-request-body

            part?
            part-headers
            part-body))

(define-record-type <part>
  (make-part headers body)
  part?
  (headers part-headers)
  (body part-body))

(define (parse-form-part part)
  "Break the PART string at the first empty line and return a part
record object."
  (match-let
      (((prefix match suffix)
         (bytevector-partition
          (u8-list->bytevector '(13 10 13 10)) part)))
    (make-part
     (call-with-input-string
         ;; TODO: bytestring-append?
         (string-append (bytevector->string (bytevector-drop prefix 2) "ISO-8859-1")
                        "\r\n\r\n")
       read-headers)
     ;; Drop last two bytes because every part body ends with "\r\n".
     (bytevector-drop-right suffix 2))))

(define (parse-request-body request body)
  "Parse the multipart/form-data request BODY and return an alist."
  (match-let
      ((('multipart/form-data ('boundary . boundary))
        (assoc-ref (request-headers request) 'content-type)))
    (map parse-form-part
         (split-parts (string-append "--" boundary) body))))

(define (split-parts boundary payload)
  "Split the bytevector PAYLOAD containing the request body at the
given BOUNDARY string.  Return a list of bytevectors."

  (define boundbv  (string->bytevector boundary (latin-1-codec)))
  (define boundlen (bytevector-length boundbv))

  (let loop ((rest payload)
             (parts '()))
    (match-let
        (((prefix match suffix)
          (bytevector-partition boundbv rest)))
      (if suffix
          (loop suffix
                (cons prefix parts))
          ;; The last part is always empty
          (cdr (reverse parts))))))
