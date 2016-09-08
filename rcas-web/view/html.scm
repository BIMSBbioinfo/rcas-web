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

(define-module (rcas-web view html)
  #:export (index))

(define* (layout #:key (head '()) (body '()))
  `((doctype "html")
    (html
     (head
      (title "RCAS")
      (meta (@ (http-equiv "Content-Type") (content "text/html; charset=UTF-8")))
      (meta (@ (http-equiv "Content-Language") (content "en")))
      (meta (@ (name "author") (content "Ricardo Wurmus")))
      (meta (@ (name "viewport") (content "width=device-width")))
      (link
       (@ (rel "stylesheet")
          (media "screen")
          (type "text/css")
          (href "css/reset.css")))
      (link
       (@ (rel "stylesheet")
          (media "screen")
          (type "text/css")
          (href "css/screen.css"))))
     (body ,@body))))

(define uploader-default-template
  '(script
    (@ (type "text/template")
       (id "qq-template"))
    (div (@ (class "qq-uploader-selector qq-uploader")
            (qq-drop-area-text "Drop files here"))
         (div (@ (class "qq-total-progress-bar-container-selector qq-total-progress-bar-container"))
              (div (@ (role "progressbar")
                      (aria-valuenow "0")
                      (aria-valuemin "0")
                      (aria-valuemax "100")
                      (class "qq-total-progress-bar-selector qq-progress-bar qq-total-progress-bar"))))
         (div (@ (class "qq-upload-drop-area-selector qq-upload-drop-area")
                 (qq-hide-dropzone "true"))
              (span (@ (class "qq-upload-drop-area-text-selector"))))
         (div (@ (class "qq-upload-button-selector qq-upload-button"))
              (div "Upload a file"))
         (span (@ (class "qq-drop-processing-selector qq-drop-processing"))
               (span "Processing dropped files...")
               (span (@ (class "qq-drop-processing-spinner-selector qq-drop-processing-spinner"))))
         (ul (@ (class "qq-upload-list-selector qq-upload-list")
                (aria-live "polite")
                (aria-relevant "additions removals"))
             (li
              (div (@ (class "qq-progress-bar-container-selector"))
                   (div (@ (role "progressbar")
                           (aria-valuenow "0")
                           (aria-valuemin "0")
                           (aria-valuemax "100")
                           (class "qq-progress-bar-selector qq-progress-bar"))))
              (span (@ (class "qq-upload-spinner-selector qq-upload-spinner")))
              (span (@ (class "qq-upload-file-selector qq-upload-file")))
              (span (@ (class "qq-edit-filename-icon-selector qq-edit-filename-icon")
                       (aria-label "Edit filename")))
              (input (@ (class "qq-edit-filename-selector qq-edit-filename")
                        (tabindex "0")
                        (type "text")))
              (span (@ (class "qq-upload-size-selector qq-upload-size")))
              (button (@ (type "button")
                         (class "qq-btn qq-upload-cancel-selector qq-upload-cancel"))
                      "Cancel")
              (button (@ (type "button")
                         (class "qq-btn qq-upload-retry-selector qq-upload-retry"))
                      "Retry")
              (button (@ (type "button")
                         (class "qq-btn qq-upload-delete-selector qq-upload-delete"))
                      "Delete")
              (span (@ (role "status")
                       (class "qq-upload-status-text-selector qq-upload-status-text")))))
         (dialog (@ (class "qq-alert-dialog-selector"))
                 (div (@ (class "qq-dialog-message-selector")))
                 (div (@ (class "qq-dialog-buttons"))
                      (button (@ (type "button")
                                 (class "qq-cancel-button-selector"))
                              "Close")))
         (dialog (@ (class "qq-confirm-dialog-selector"))
                 (div (@ (class "qq-dialog-message-selector")))
                 (div (@ (class "qq-dialog-buttons"))
                      (button (@ (type "button")
                                 (class "qq-cancel-button-selector"))
                              "No")
                      (button (@ (type "button")
                                 (class "qq-ok-button-selector"))
                              "Yes")))
         (dialog (@ (class "qq-prompt-dialog-selector"))
                 (div (@ (class "qq-dialog-message-selector")))
                 (input (@ (type "text")))
                 (div (@ (class "qq-dialog-buttons"))
                      (button (@ (type "button")
                                 (class "qq-cancel-button-selector"))
                              "Cancel")
                      (button (@ (type "button")
                                 (class "qq-ok-button-selector"))
                              "Okay"))))))

(define index
  (layout
   #:head
   `(;; The "Fine Uploader" library 5.11.5 taken from here:
     ;; http://fineuploader.com/
     (link
      (@ (rel "stylesheet")
         (media "screen")
         (type "text/css")
         (href "fine-uploader/fine-uploader-new.css"))))
   #:body
   `((div (@ (id "fine-uploader")))
     (script
      (@ (type "text/javascript")
         (src "fine-uploader/fine-uploader.js")))
     ,uploader-default-template
     (script
      (@ (type "text/javascript")
         (src "js/init-fine-uploader.js"))))))
