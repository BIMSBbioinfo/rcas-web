# rcas-web - Web interface for RCAS
# Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
#
# This file is part of rcas-web.
#
# rcas-web is free software; see LICENSE file for details.

bin_SCRIPTS =						\
  scripts/rcas-web-server			\
  scripts/rcas-web-worker

moddir = $(prefix)/share/guile/site/@GUILE_EFFECTIVE_VERSION@

dist_mod_DATA =						\
  rcas/utils/worker.scm				\
  rcas/utils/r.scm					\
  rcas/utils/jobs.scm				\
  rcas/web/controller/upload.scm	\
  rcas/web/controller/result.scm	\
  rcas/web/render.scm				\
  rcas/web/view/html.scm			\
  rcas/web/controller.scm			\
  rcas/web/util.scm					\
  rcas/web/server.scm				\
  rcas/web/bytevectors.scm			\
  rcas/web/sxml.scm					\
  rcas/web/multipart.scm			\
  rcas/config.scm
