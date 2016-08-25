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

(define-module (rcas-web multipart)
  #:use-module (rcas-web bytevectors)
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
         (string-append (bytevector->string prefix "ISO-8859-1")
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

;; TODO: reimplement in terms of bytevector-partition
(define (split-parts boundary payload)
  "Split the bytevector PAYLOAD containing the request body at the
given BOUNDARY string.  Return a list of bytevectors."

  (define boundbv  (string->bytevector boundary (latin-1-codec)))
  (define boundlen (bytevector-length boundbv))

  ;; Skip over the boundary and the "\r\n" (or "--") after it if we
  ;; are at the beginning of the boundary string.
  (define (at-boundary? port)
    (let ((read (get-bytevector-n port boundlen)))
      (cond
       ((and (not (eof-object? read))
             (bytevector=? read boundbv))
        ;; throw away the next two bytes
        (get-bytevector-n port 2)
        #t)
       (else (when (bytevector? read)
               (unget-bytevector port read))
             #f))))

  ;; Read bytes from PORT until hitting a newline character.  Return
  ;; read bytes as a bytevector.
  (define (read-until-boundary port)
    (call-with-values open-bytevector-output-port
      (lambda (out get-bytevector)
        (let loop ()
          ;; TODO: read more than one byte at a time?
          (let ((byte (get-u8 port)))
            (if (eof-object? byte)
                (get-bytevector)
                (begin
                  (put-u8 out byte)
                  (if (and (equal? 10 byte)
                           (at-boundary? port))
                      (get-bytevector)
                      (loop)))))))))

  (call-with-port
   (open-bytevector-input-port payload)
   (lambda (port)
     (at-boundary? port) ; skip first boundary
     (let loop ((lst '()))
       (let ((part (read-until-boundary port)))
         (if (or (eof-object? part)
                 (zero? (bytevector-length part)))
             ;; The last part is always empty, so throw it away
             (reverse (cdr lst))
             (loop (cons part lst))))))))
