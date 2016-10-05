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
  #:export (index
            result-page
            invalid-result))

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
          (href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css")
          (integrity "sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u")
          (crossorigin"anonymous")))
      (link
       (@ (rel "stylesheet")
          (href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css")
          (integrity "sha384-rHyoN1iRsVXV4nD0JutlnGaslCJuC7uwjduW9SVrLvRYooPp2bWYgmgJQIXwl/Sp")
          (crossorigin "anonymous")))
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

(define* (jumbotron #:optional (body '()))
  `(div (@ (class "jumbotron"))
        (div (@ (class "container"))
             (a (@ (href "/"))
                (h1 "RCAS"
                    (small "RNA Centric Annotation System")))
             ,body)))

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
   `(,(jumbotron)
     (div (@ (class "container"))
          (div (@ (id "about")
                  (class "row"))
           (p "RCAS facilitates biological discovery from target
regions located by methods such as Clip-Seq.  RCAS automatically
provides dynamic annotations for custom input files that contain
transcriptomic target regions.")
           (p "Provided with target regions in BED format, RCAS
automatically summarizes annotated features that overlap with targets,
and performs enrichment analysis of motifs, Gene Ontology (GO) terms
and gene set.  The final report of RCAS consists of high-quality
dynamic figures and tables, which are readily applicable for
publications or other academic usage."))
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
           (div (@ (class "form-group row"))
                (label (@ (class "col-md-2 control-label"))
                       "Analysis modules")
                (div (@ (class "col-md-6"))
                     (div (@ (class "checkbox"))
                          (label
                           (input (@ (type "checkbox")
                                     (id   "annotation-summary")
                                     (name "annotation-summary")))
                           "Provide annotation summaries from overlap operations"))
                     (div (@ (class "checkbox"))
                          (label
                           (input (@ (type "checkbox")
                                     (id   "go-analysis")
                                     (name "go-analysis")))
                           "Run GO term enrichment analysis"))
                     (div (@ (class "checkbox"))
                          (label
                           (input (@ (type "checkbox")
                                     (id   "msigdb-analysis")
                                     (name "msigdb-analysis")))
                           "Run gene set enrichment analysis"))
                     (div (@ (class "checkbox"))
                          (label
                           (input (@ (type "checkbox")
                                     (id   "motif-analysis")
                                     (name "motif-analysis")))
                           "Run motif analysis"))))
           (div (@ (class "form-group"))
                (p "For very large input BED files it may make sense
to downsample the data.  If a positive integer value is provided, the
number of intervals in the input BED file will be randomly downsampled
to include intervals up to the specified number.  The input will be
downsampled only if this value is larger than zero and less than the
total number of input intervals.")
                (label (@ (class "col-md-2 control-label"))
                       "Intervals")
                (div (@ (class "col-md-6"))
                     (div (@ (class "checkbox"))
                          (input (@ (type "text")
                                    (id   "sample-n")
                                    (name "sample-n")
                                    (placeholder "intervals"))))))
           (button (@ (type "submit")
                      (class "btn btn-lg btn-primary btn-block"))
                   "Run RCAS")))
     (script
      (@ (type "text/javascript")
         (src "fine-uploader/fine-uploader.js")))
     ,uploader-default-template
     (script
      (@ (type "text/javascript")
         (src "js/init-fine-uploader.js"))))))

(define (result-page id status ago options errors output result refresh?)
  (layout
   #:head
   (if refresh?
       `((meta (@ (http-equiv "refresh")
                  (content ,(string-append "30; URL=/result/" id)))))
       '())
   #:body
   `(,(jumbotron
       `(p "Results page for file "
           (strong ,id)))
     (div (@ (class "container"))
          (h2 "Details")
          (p (strong "Status: ")
             ,(format #f "~a (since ~a)" status ago))
          ,(if (string-prefix? "success" status)
               `(p (a (@ (href ,(string-append "/result/"
                                               id "/report")))
                      "Access the RCAS report here."))
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
                      "How about running RCAS on another file?")))))))

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
