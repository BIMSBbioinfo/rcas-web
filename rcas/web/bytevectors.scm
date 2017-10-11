;;; rcas-web - Web interface for RCAS
;;; Copyright © 2016, 2017  Ricardo Wurmus <rekado@elephly.net>
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

(define-module (rcas web bytevectors)
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
  (define separator-first (bytevector-u8-ref separator 0))

  (define (at-separator? pos)
    (let ((end (+ pos (- separator-length 1))))
      (if (< end bv-length)
          (let loop ((i 0))
            (let ((offset (+ pos i)))
              (if (eqv? (bytevector-u8-ref bv offset)
                        (bytevector-u8-ref separator i))
                  (if (< offset end)
                      (loop (1+ i))
                      #t)
                  #f)))
          #f)))

  (if (< bv-length separator-length)
      (list bv #f #f)
      (let ((found (let scan-at ((pos 0))
                     (let ((byte (bytevector-u8-ref bv pos)))
                       (if (and (eqv? byte separator-first)
                                (at-separator? pos))
                           pos
                           (let ((next-pos (1+ pos)))
                             (if (< next-pos bv-length)
                                 (scan-at next-pos)
                                 #f)))))))
        (if found
            (let* ((prefix (make-bytevector found))
                   (suffix-start  (+ found separator-length))
                   (suffix-length (- bv-length suffix-start))
                   (suffix (make-bytevector suffix-length)))
              (bytevector-copy! bv 0 prefix 0 found)
              (bytevector-copy! bv suffix-start suffix 0 suffix-length)
              (list prefix separator suffix))
            (list bv #f #f)))))
