;;; rcas-web - Web interface for RCAS
;;; Copyright © 2016, 2017, 2019 Ricardo Wurmus <rekado@elephly.net>
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
             (gnu packages guile-xyz)
             (gnu packages pkg-config))

(define-public rcas-web/devel
  (package (inherit rcas-web)
    (name "rcas-web-development")
    (source #f)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ,@(package-native-inputs rcas-web)))))

rcas-web/devel
