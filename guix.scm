;;; rcas-web - Web interface for RCAS
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;;
;;; This file is part of rcas-web.
;;;
;;; rcas-web is free software; see COPYING file for details.

;;; Run the following command to enter a development environment for
;;; RCAS web:
;;;
;;;  $ guix environment -l guix.scm

(use-modules ((guix licenses) #:prefix license:)
             (guix packages)
             (guix download)
             (guix git-download)
             (guix utils)
             (guix build-system gnu)
             (guix build-system r)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages bioinformatics)
             (gnu packages statistics)
             (gnu packages guile)
             (gnu packages pkg-config))

;; The latest release of bedtools (version 2.25.0 as of this writing)
;; segfaults on intersect, so we need a more recent version.  We
;; cannot use the latest commit, because that removes command line
;; options from "fastaFromBed".  Another possible version to use is
;; 2.24.x, which does not exhibit this behaviour.
(define-public bedtools/patched
  (let ((commit "2bbe124cbc6e8df393d2ca21b0b29411aefdcf8f"))
    (package
      (inherit bedtools)
      (name "bedtools")
      (version "2.25.0-50-g2bbe124")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/arq5x/bedtools2.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1zhzm4kkpldc8l2fiicgb8aywcbcngy21baxpdi3cdcrjqmcfj17"))))
      (arguments
       (substitute-keyword-arguments (package-arguments bedtools)
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'check
               (lambda _
                 (with-directory-excursion "test"
                   (zero? (system* "bash" "test.sh"))))))))))))

(define-public r-rcas
  (package
    (name "r-rcas")
    (version "0.99.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BIMSBbioinfo/RCAS/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1s6afz61fppvyxmgm9dbc6riyc879797frfs4cffjj0fd9fn6qhz"))))
    (build-system r-build-system)
    (native-inputs
     `(("r-roxygen2" ,r-roxygen2)
       ("r-knitr" ,r-knitr)))
    (propagated-inputs
     `(("r-data-table" ,r-data-table)
       ("r-biomart" ,r-biomart)
       ("r-org-hs-eg-db" ,r-org-hs-eg-db)
       ("r-org-ce-eg-db" ,r-org-ce-eg-db)
       ("r-org-dm-eg-db" ,r-org-dm-eg-db)
       ("r-org-mm-eg-db" ,r-org-mm-eg-db)
       ("r-bsgenome-hsapiens-ucsc-hg19"
        ,r-bsgenome-hsapiens-ucsc-hg19)
       ("r-bsgenome-mmusculus-ucsc-mm9"
        ,r-bsgenome-mmusculus-ucsc-mm9)
       ("r-bsgenome-celegans-ucsc-ce6"
        ,r-bsgenome-celegans-ucsc-ce6)
       ("r-bsgenome-dmelanogaster-ucsc-dm3"
        ,r-bsgenome-dmelanogaster-ucsc-dm3)
       ("r-topgo" ,r-topgo)
       ("r-dt" ,r-dt)
       ("r-plotly" ,r-plotly)
       ("r-doparallel" ,r-doparallel)
       ("r-motifrg" ,r-motifrg)
       ("r-genomation" ,r-genomation)
       ("r-genomicfeatures" ,r-genomicfeatures)
       ("r-rtracklayer" ,r-rtracklayer)
       ("r-rmarkdown" ,r-rmarkdown)))
    (synopsis "RNA-centric annotation system")
    (description
     "RCAS aims to be a standalone RNA-centric annotation system
that provides intuitive reports and publication-ready graphics.  This
package provides the R library implementing most of the pipeline's
features.")
    (home-page "https://github.com/BIMSBbioinfo/RCAS")
    (license license:expat)))

(define-public rcas-web
  (package
    (name "rcas-web")
    (version "0.0.1")
    (source #f)
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'autoconf
           (lambda _
             (zero? (system* "autoreconf" "-vif")))))))
    (inputs
     `(("r" ,r)
       ("r-rcas" ,r-rcas)
       ("guile-next" ,guile-next)
       ("guile-json" ,guile2.2-json)
       ("guile-redis" ,guile2.2-redis)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (synopsis "Web interface for RNA-centric annotation system (RCAS)")
    (description
     "RCAS aims to be a standalone RNA-centric annotation system that
provides intuitive reports and publication-ready graphics.")
    (home-page "https://github.com/BIMSBbioinfo/rcas-web")
    (license license:agpl3+)))

rcas-web
