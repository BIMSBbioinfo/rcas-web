# rcas-web - Web interface for RCAS
# Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
#
# This file is part of rcas-web.
#
# rcas-web is free software; see LICENSE file for details.

SCMGOBJECTS = $(filter %.scm, $(SSOURCES))
SOURCES     = $(SCMGOBJECTS)
GOBJECTS    = $(SCMGOBJECTS:%.scm=%.go)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

CLEANFILES = $(GOBJECTS)
EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go

.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"


bin_SCRIPTS =						\
  scripts/rcas-web

moddir = $(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir  = $(moddir)
assetsdir = $(datadir)/@PACKAGE@

nobase_dist_assets_DATA = \
  assets/css/reset.css											\
  assets/css/screen.css											\
  assets/fine-uploader/loading.gif								\
  assets/fine-uploader/placeholders/not_available-generic.png	\
  assets/fine-uploader/placeholders/waiting-generic.png			\
  assets/fine-uploader/dnd.js									\
  assets/fine-uploader/edit.gif									\
  assets/fine-uploader/fine-uploader.js							\
  assets/fine-uploader/processing.gif							\
  assets/fine-uploader/fine-uploader.css						\
  assets/fine-uploader/trash.gif								\
  assets/fine-uploader/pause.gif								\
  assets/fine-uploader/continue.gif								\
  assets/fine-uploader/fine-uploader-new.css					\
  assets/fine-uploader/retry.gif								\
  assets/js/init-fine-uploader.js								\
  assets/js/rcas.js												\
  assets/webfonts/LinLibertine_R.woff

SSOURCES =							\
  rcas/ui.scm						\
  rcas/utils/worker.scm				\
  rcas/utils/r.scm					\
  rcas/utils/report.scm				\
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
