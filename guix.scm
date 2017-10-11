;;; rcas-web - Web interface for RCAS
;;; Copyright © 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
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
             (guix utils)
             (guix build-system gnu)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages bioinformatics)
             (gnu packages statistics)
             (gnu packages guile)
             (gnu packages pkg-config))

(define-public rcas-web/devel
  (package (inherit rcas-web)
    (name "rcas-web-development")
    (source #f)
    (build-system gnu-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments rcas-web)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'configure 'autoconf
             (lambda _
               (zero? (system* "autoreconf" "-vif"))))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ,@(package-native-inputs rcas-web)))))

rcas-web/devel
