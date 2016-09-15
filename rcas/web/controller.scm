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

(define-module (rcas web controller)
  #:use-module (ice-9 match)
  #:use-module (web request)
  #:use-module (rcas web controller upload)
  #:use-module (rcas web controller result)
  #:use-module (rcas web render)
  #:use-module (rcas web view html)
  #:export (controller))

(define (controller request body)
  (match-lambda
   ((GET)
    (render-html index))
   ((POST "uploads" ...)
    (render-json (upload-handler request body)))
   ((GET "result" id)
    (render-html (result-handler id)))
   ((GET "result" id "report")
    (render-report id))
   ((GET path ...)
    (render-static-asset path))))
