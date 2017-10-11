;;; rcas-web - Web interface for RCAS
;;; Copyright © 2016, 2017  Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2014  David Thompson <davet@gnu.org>
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

;; This code was snarfed from David Thompson's guix-web.

(define-module (rcas web render)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 binary-ports)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (json)
  #:use-module (rcas config)
  #:use-module (rcas web sxml)
  #:use-module (rcas web util)
  #:export (render-static-asset
            render-report
            render-report-asset
            render-html
            render-json
            not-found
            unprocessable-entity
            created
            redirect))

(define file-mime-types
  '(("css" . (text/css))
    ("js"  . (text/javascript))
    ("png" . (image/png))
    ("gif" . (image/gif))
    ("woff" . (application/font-woff))
    ("ttf"  . (application/octet-stream))
    ("html" . (text/html))))

(define (render-report id)
  (render-report-asset id
                       (list (string-append id ".RCAS.report.html"))))

(define (render-report-asset id asset-path)
  ;; Allow only lowercase letters, numbers, and dashes in report
  ;; identifiers.
  (let* ((valid-chars (char-set-adjoin (char-set-union char-set:lower-case
                                                       char-set:digit)
                                       #\-))
         (clean-id (string-filter valid-chars id)))
    (render-static-file (string-append (assoc-ref %config 'results-dir)
                                       "/" clean-id)
                        asset-path)))

(define (render-static-asset path)
  (render-static-file (assoc-ref %config 'assets-dir) path))

(define (render-static-file root path)
  ;; PATH is a list of path components
  (let ((file-name (string-join (cons* root path) "/")))
    (if (and (not (any (cut string-contains <> "..") path))
             (file-exists? file-name)
             (not (directory? file-name)))
        (list `((content-type . ,(assoc-ref file-mime-types
                                            (file-extension file-name))))
              (call-with-input-file file-name get-bytevector-all))
        (not-found (build-uri 'http
                              #:host (assoc-ref %config 'host)
                              #:port (assoc-ref %config 'port)
                              #:path (string-join path "/" 'prefix))))))

(define (render-html sxml)
  (list '((content-type . (text/html)))
        (lambda (port)
          (sxml->html sxml port))))

(define (render-json json)
  (list '((content-type . (application/json)))
        (lambda (port)
          (scm->json json port))))

(define (not-found uri)
  (list (build-response #:code 404)
        (string-append "Resource not found: " (uri->string uri))))

(define (unprocessable-entity)
  (list (build-response #:code 422)
        ""))

(define (created)
  (list (build-response #:code 201)
        ""))

(define (redirect path)
  (let ((uri (build-uri 'http
                        #:host (assoc-ref %config 'host)
                        #:port (assoc-ref %config 'port)
                        #:path (string-append
                                "/" (encode-and-join-uri-path path)))))
    (list (build-response
           #:code 301
           #:headers `((content-type . (text/html))
                       (location . ,uri)))
          (format #f "Redirect to ~a" (uri->string uri)))))
