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

(define-module (rcas-web bytevectors)
  #:use-module (ice-9 iconv)
  #:use-module (rnrs io ports)
  #:use-module ((ice-9 binary-ports) #:select (unget-bytevector))
  #:use-module (rnrs bytevectors)
  #:export (bytevector-drop
            bytevector-drop-right
            bytevector-last
            bytevector-partition))

(define empty-bv
  (make-bytevector 0))

(define (bytevector-drop bv n)
  "Drop the first N bytes of the bytevector BV."
  (let ((len (bytevector-length bv)))
    (if (>= n len)
        empty-bv
        (let* ((new-length (- len n))
               (target (make-bytevector new-length)))
          (bytevector-copy! bv n target 0 new-length)
          target))))

(define (bytevector-drop-right bv n)
  "Drop the last N bytes of the bytevector BV."
  (let ((len (bytevector-length bv)))
    (if (>= n len)
        empty-bv
        (let* ((new-length (- len n))
               (target (make-bytevector new-length)))
          (bytevector-copy! bv 0 target 0 new-length)
          target))))

(define (bytevector-last bv)
  "Return the last byte of the bytevector BV."
  (bytevector-u8-ref bv (- (bytevector-length bv) 1)))

(define (bytevector-partition separator bv)
  "Find the bytevector SEPARATOR in the bytevector BV and return a
list of three values: the prefix, the separator, and the suffix.  If
there is no match the list will only contain the prefix and return #f
for the remaining values."
  (define bv-length (bytevector-length bv))
  (define separator-length (bytevector-length separator))
  (define (at-separator-end? pos)
    (let ((beg (- pos (- separator-length 1))))
      (if (>= beg 0)
          (let* ((check (make-bytevector separator-length)))
            (bytevector-copy! bv beg check 0 separator-length)
            (bytevector=? check separator))
          #f)))

  (if (< bv-length separator-length)
      (list bv #f #f)
      (let ((found (let scan-at ((pos 0))
                     (let ((byte (bytevector-u8-ref bv pos)))
                       (if (and (equal? byte (bytevector-last separator))
                                (at-separator-end? pos))
                           (1+ (- pos separator-length))
                           (let ((next-pos (1+ pos)))
                             (if (< next-pos bv-length)
                                 (scan-at next-pos)
                                 #f)))))))
        (if found
            (let* ((prefix (make-bytevector found))
                   (suffix-length (- bv-length
                                     (+ found separator-length)))
                   (suffix (make-bytevector suffix-length)))
              (bytevector-copy! bv 0 prefix 0 found)
              (bytevector-copy! bv (+ found separator-length)
                                suffix 0 suffix-length)
              (list prefix separator suffix))
            (list bv #f #f)))))

(define (bytevector-partition/slow separator bv)
  "Find the bytevector SEPARATOR in the bytevector BV and return a
list of three values: the prefix, the separator, and the suffix.  If
there is no match the list will only contain the prefix and return #f
for the remaining values."
  (define separator-length
    (bytevector-length separator))

  (define (at-separator-end? port)
    ;; Rewind to check if we were at the end of the separator.
    (seek port (- separator-length) SEEK_CUR)
    (let ((read (get-bytevector-n port separator-length)))
      (bytevector=? read separator)))

  (define (read-until-match port)
    (call-with-values open-bytevector-output-port
      (lambda (out get-bytevector)
        ;; Skip over the initial chunk to avoid seeking into past the
        ;; beginning.
        (let ((initial-chunk
               (get-bytevector-n port (- separator-length 1))))
          (put-bytevector out initial-chunk))
        (let loop ()
          ;; TODO: read more than one byte at a time?
          (let ((byte (get-u8 port)))
            (if (eof-object? byte)
                (get-bytevector)
                (begin
                  (put-u8 out byte)
                  (if (and (equal? byte
                                   (bytevector-last separator))
                           (at-separator-end? port))
                      (get-bytevector)
                      (loop)))))))))

  (call-with-port
   (open-bytevector-input-port bv)
   (lambda (port)
     (let* ((first (read-until-match port))
            (rest  (get-bytevector-all port)))
       (if (eof-object? rest)
           ;; didn't match
           (list first #f #f)
           (list (bytevector-drop-right first separator-length)
                 separator rest))))))
