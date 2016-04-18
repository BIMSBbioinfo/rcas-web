;;; RCAS -- standalone RNA centric annotation system
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;;
;;; This file is part of RCAS.
;;;
;;; RCAS is free software; see LICENSE file for details.

;;; Run the following command to enter a development environment for
;;; RCAS:
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
             (gnu packages statistics))

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
  (let ((commit "384951de1d2d4986d0a1eb49f7a877775525117a")
        (revision "2"))
    (package
      (name "r-rcas")
      (version (string-append "0.0.0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/BIMSBbioinfo/RCAS.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "00hsr11cdkrkkgqfvsrdqdw10nni3l2qq4ykarmcr81ls86m28r2"))))
      (build-system r-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _ (chdir "RCAS") #t)))))
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
      (license license:expat))))

(package
  (name "rcas")
  (version "0.0.0")
  (source #f)
  (build-system gnu-build-system)
  (inputs
   `(("bedtools" ,bedtools/patched)
     ("r" ,r)
     ("r-data-table" ,r-data-table)
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
  (native-inputs
   `(("autoconf" ,autoconf)
     ("automake" ,automake)))
  (synopsis "RNA-centric annotation system")
  (description
   "RCAS aims to be a standalone RNA-centric annotation system that
provides intuitive reports and publication-ready graphics.")
  (home-page "TODO")
  (license license:expat))
