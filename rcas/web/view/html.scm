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

(define-module (rcas web view html)
  #:use-module (rcas utils report) ; for msigdb-options
  #:export (index
            result-page
            invalid-result
            cannot-download-report
            javascript-license-information))

(define* (layout #:key (head '()) (body '()))
  `((doctype "html")
    (html
     (head
      (title "RCAS")
      (meta (@ (http-equiv "Content-Type") (content "text/html; charset=UTF-8")))
      (meta (@ (http-equiv "Content-Language") (content "en")))
      (meta (@ (name "author") (content "Ricardo Wurmus")))
      (meta (@ (name "viewport")
               (content "width=device-width, initial-scale=1")))
      (script
       (@ (type "text/javascript")
          (src "/js/rcas.js")))
      (link
       (@ (rel "stylesheet")
          (media "screen")
          (type "text/css")
          (href "/css/reset.css")))
      (link
       (@ (rel "stylesheet")
          (media "screen")
          (type "text/css")
          (href "/css/bootstrap.css")))
      (link
       (@ (rel "stylesheet")
          (media "screen")
          (type "text/css")
          (href "/css/bootstrap-theme.css")))
      ,@head
      (link
       (@ (rel "stylesheet")
          (media "screen")
          (type "text/css")
          (href "/css/screen.css"))))
     (body ,@body))))

(define uploader-default-template
  '(script
    (@ (type "text/template")
       (id "qq-template"))
    (div (@ (class "qq-uploader-selector qq-uploader"))
         (div (@ (class "qq-upload-button-selector btn btn-default"))
              (div "Select a file for upload"))
         (span (@ (class "qq-drop-processing-selector qq-drop-processing"))
               (span "Processing dropped file...")
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
              (span (@ (class "qq-upload-size-selector qq-upload-size")))
              (button (@ (type "button")
                         (class "qq-btn qq-upload-cancel-selector qq-upload-cancel"))
                      "Cancel")
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

(define footer
  `(footer
    (p "RCAS was developed by "
       (a (@ (href "http://bioinformatics.mdc-berlin.de/team.html#bora-uyar-phd"))
          "Bora Uyar")
       ", "
       (a (@ (href "http://bioinformatics.mdc-berlin.de/team.html#dilmurat-yusuf-phd"))
          "Dilmurat Yusuf")
       ", "
       (a (@ (href "http://bioinformatics.mdc-berlin.de/team.html#ricardo-wurmus"))
          "Ricardo Wurmus")
       ", and "
       (a (@ (href "http://bioinformatics.mdc-berlin.de/team.html#altuna-akalin-phd"))
          "Altuna Akalin")
       ".")
    (p "In case of technical issues with this web interface "
       (a (@ (href "mailto:ricardo.wurmus@mdc-berlin.de"))
          "please contact Ricardo Wurmus")
       ".")
    (p (a (@ (href "/javascript")
             (data-jslicense "1"))
          "JavaScript license information"))))

(define* (jumbotron #:optional (body '()))
  `(div (@ (class "jumbotron"))
        (div (@ (class "container"))
             (a (@ (href "/"))
                (h1 "RCAS"
                    (small "RNA Centric Annotation System")))
             ,body)))

(define (index)
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
   `(,(jumbotron)
     (div (@ (class "container"))
          (div (@ (id "about")
                  (class "row"))
           (p "RCAS facilitates biological discovery from target
regions located by methods such as Clip-Seq.  RCAS automatically
provides dynamic annotations for custom input files that contain
transcriptomic target regions.")
           (p "Visit "
              (a (@ (href "http://bioconductor.org/packages/release/bioc/html/RCAS.html"))
                 "the RCAS Bioconductor page")
              " for additional documentation."))
          (form (@ (id "qq-form")
                   (class "row"))
           (div (@ (class "form-group row"))
                (label (@ (class "col-md-2 control-label"))
                       "BED file to analyze")
                (div (@ (class "col-md-6")
                        (id "fine-uploader"))))
           (div (@ (class "form-group row"))
                (label (@ (class "col-md-2 control-label")
                          (for "genome-version"))
                       "Select genome")
                (div (@ (class "col-md-6"))
                     (select (@ (id "genome-version")
                                (name "genome-version")
                                (class "form-control"))
                       (option (@ (value "hg19"))
                               "Human (hg19)")
                       (option (@ (value "mm9"))
                               "Mouse (mm9)")
                       (option (@ (value "ce10"))
                               "Worm (ce10)")
                       (option (@ (value "dm3"))
                               "Fly (dm3)"))))
           ,(let ((msigdb (msigdb-options)))
              (if msigdb
                  `(div (@ (class "form-group row"))
                        (label (@ (class "col-md-2 control-label")
                                  (for "genome-version"))
                               "Select gene set")
                        (div (@ (class "col-md-6"))
                             (select (@ (id "msigdb-file-path")
                                        (name "msigdb-file-path")
                                        (class "form-control"))
                               ,@msigdb)))
                  '()))
           (div (@ (class "form-group row"))
                (label (@ (class "col-md-2 control-label"))
                       "Analysis modules")
                (div (@ (class "col-md-6"))
                     (div (@ (class "checkbox"))
                          (label
                           (input (@ (type "checkbox")
                                     (id   "annotation-summary")
                                     (name "annotation-summary")
                                     (checked "true")))
                           "Provide annotation summaries from overlap operations"))
                     (div (@ (class "checkbox"))
                          (label
                           (input (@ (type "checkbox")
                                     (id   "go-analysis")
                                     (name "go-analysis")
                                     (checked "true")))
                           "Run GO term enrichment analysis"))
                     (div (@ (class "checkbox"))
                          (label
                           (input (@ (type "checkbox")
                                     (id   "msigdb-analysis")
                                     (name "msigdb-analysis")
                                     (checked "true")))
                           "Run gene set enrichment analysis"))
                     (div (@ (class "checkbox"))
                          (label
                           (input (@ (type "checkbox")
                                     (id   "motif-analysis")
                                     (name "motif-analysis")
                                     (checked "true")))
                           "Run motif analysis"))))
           (div (@ (class "form-group row"))
                (label (@ (class "col-md-2 control-label"))
                       "Downsampling")
                (div (@ (class "col-md-6"))
                     (div (@ (class "form-group"))
                          (input (@ (type "text")
                                    (id   "sample-n")
                                    (name "sample-n")
                                    (placeholder "sample size"))))))
           (button (@ (type "submit")
                      (class "btn btn-lg btn-primary btn-block"))
                   "Run RCAS")))
     ,footer
     (script
      (@ (type "text/javascript")
         (src "fine-uploader/fine-uploader.js")))
     ,uploader-default-template
     (script
      (@ (type "text/javascript")
         (src "js/init-fine-uploader.js"))))))

(define (javascript-license-information)
  (layout
   #:body
   `(,(jumbotron)
     (div (@ (class "container"))
          (h2 "JavaScript license information")
          (p "All JavaScript code used by RCAS is free software.  The
following table lists all scripts and their respective licenses.")
          (p "For more information on JavaScript and free software see "
             (a (@ (href "https://www.gnu.org/philosophy/javascript-trap.html"))
                "The JavaScript Trap")
             ".")
          (table (@ (id "jslicense-labels1"))
                 (tr (td (a (@ (href "js/init-fine-uploader.js"))
                            "init-fine-uploader.js"))
                     (td (a (@ (href "https://gnu.org/licenses/agpl-3.0.html"))
                            "GNU-AGPL-3.0-or-later")))
                 (tr (td (a (@ (href "js/rcas.js"))
                            "rcas.js"))
                     (td (a (@ (href "https://gnu.org/licenses/agpl-3.0.html"))
                            "GNU-AGPL-3.0-or-later")))
                 (tr (td (a (@ (href "fine-uploader/fine-uploader.js"))
                            "fine-uploader.js"))
                     (td (a (@ (href "http://www.jclark.com/xml/copying.txt"))
                            "Expat")))))
     ,footer)))

(define* (result-page id status ago options errors output result refresh?
                      #:optional (progress 0))
  (layout
   #:head
   `((script (@ (type "text/javascript"))
             "window.onload = RCAS.galaxyInit;")
     ,@(if refresh?
           `((script (@ (type "text/javascript"))
                      "refresh = window.setTimeout(function(){window.location.href=window.location.href},30000);"))
           '()))

   #:body
   `(,(jumbotron
       `(p "Results page for file "
           (strong ,id)))
     (div (@ (class "container"))
          (h2 "Details")
          (p (strong "Status: ")
             ,(format #f "~a (since ~a)" status ago))
          ,@(if (string-prefix? "success" status)
                `((p (@ (class "galaxy"))
                     (a (@ (class "btn btn-primary")
                           (href "#")
                           (onclick
                            ,(string-append "RCAS.galaxySend(this, '"
                                            id "')")))
                        "Send report to Galaxy"))
                  (p (@ (class "galaxy-hide"))
                     (a (@ (href ,(string-append "/result/"
                                                 id "/report")))
                        "View the RCAS report here."))
                  (p (@ (class "galaxy-hide"))
                     (a (@ (href ,(string-append "/result/"
                                                 id "/download")))
                        "Download the RCAS report here.")))
                '())
          ,@(if (string-prefix? "processing" status)
                `((div (@ (class "progress"))
                       (div (@ (class "progress-bar progress-bar-success progress-bar-striped active")
                               (role "progressbar")
                               (aria-valuenow progress)
                               (aria-valuemin "0")
                               (aria-valuemax "100")
                               (style ,(string-append
                                        "min-width: 2em; width: "
                                        progress "%")))
                            ,(string-append progress "%"))))
                '())
          ,options
          ,@(when output
              `((h2 "Output")
                (p (a (@ (href "javascript:;")
                         (onclick "RCAS.toggleLog('output')"))
                      "Click to toggle."))
                (pre (@ (id "output")) ,output)))
          ,@(when errors
              `((h2 "Errors and warnings")
                (p (a (@ (href "javascript:;")
                         (onclick "RCAS.toggleLog('errors')"))
                      "Click to toggle."))
                (pre (@ (id "errors")) ,errors)))
          ,(if refresh?
               '(p "This page will try to refresh in 30 seconds.")
               '(p (a (@ (href "/"))
                      "How about running RCAS on another file?"))))
     ,footer)))

(define (invalid-result id)
  (layout
   #:body
   `(,(jumbotron
       `(p "There are no results for file "
           (strong ,id)))
     (div (@ (class "container"))
          (p "The result id does not exist.  Results are only kept for
a limited amount of time.")
          (p (a (@ (href "/"))
                "How about running RCAS on another file?"))))))

(define (cannot-download-report id)
  (layout
   #:body
   `(,(jumbotron
       `(p "The report for "
           (strong ,id)
           " cannot be downloaded."))
     (div (@ (class "container"))
          (p "There was an error creating the report archive.")
          (p (a (@ (href "/"))
                "How about running RCAS on another file?"))))))
