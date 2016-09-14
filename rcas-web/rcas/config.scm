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

(define-module (rcas config)
  #:export (rcas-web-asset-dir
            rcas-web-upload-dir
            rcas-web-results-dir
            rcas-web-host
            rcas-web-port

            %Rscript))

(define rcas-web-asset-dir (string-append (getcwd) "/assets"))
(define rcas-web-upload-dir  "/tmp/rcas-uploads")
(define rcas-web-results-dir "/tmp/rcas-results")
(define rcas-web-host "localhost")
(define rcas-web-port 8080)

(define %Rscript "Rscript")