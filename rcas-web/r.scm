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

(define-module (rcas-web r)
  #:use-module (ice-9 match)
  #:use-module (rcas-web config)
  #:export (runR
            scheme->R))

;; TODO: this fails
;; (scheme->R '((file (hello . "world") (moo . (foo 10)))))
;; because (moo . (foo 10)) is a list (and also a pair).

;; We cannot do keyword args like that.  Must be with keys or assoc
;; list.

;; TODO: This is primitive and terrible but it gets the job done.
(define scheme-val->R-val
  (match-lambda
    ((? list? val)
     (if (and (> (length val) 2)
              (eqv? (car val) 'set!))
         ;; Assignment
         (string-append (scheme-val->R-val (cadr val))
                        " <- "
                        (scheme-val->R-val (caddr val)))
         ;; Function application
         (begin
           ;(format #t "fun: ~a to ~a\n" (car val) (cadr val))
           (string-append
            (symbol->string (car val)) "("
            (string-join (map scheme-val->R-val (cdr val))
                         ", ")
            ")"))))
    ((? pair? val)
     (string-append
      (scheme-val->R-val (car val))
      "="
      (scheme-val->R-val (cdr val))))
    ((? string? val)
     (string-append "\"" val "\""))
    ((? number? val)
     (number->string val))
    ((? symbol? val)
     (symbol->string val))
    ((? boolean? val)
     (if val "TRUE" "FALSE"))))

(define (scheme->R exps)
  "Convert a bunch of S-expressions EXPS to a string of R code."
  (string-join (map scheme-val->R-val exps) ";"))

(define* (runR str)
  "Run the R script provided as a string in STR."
  (system* %Rscript "-e" str))
