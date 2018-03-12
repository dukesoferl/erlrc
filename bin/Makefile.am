# put whatever (auto)make commands here, they will be included from Makefile.am

dist_bin_SCRIPTS =		\
  erlrc-make-boot-script	\
  erlrc-downgrade		\
  erlrc-eval			\
  erlrc-hooks			\
  erlrc-makeappup		\
  erlrc-start			\
  erlrc-stop			\
  erlrc-upgrade

.hidden:
	touch .hidden

erlrc_dir = /etc/erlrc.d
erlrc_appdir = /etc/erlrc.d/applications
erlrc_nodesdir = /etc/erlrc.d/nodes
dist_erlrc_app_DATA = .hidden
dist_erlrc_nodes_DATA = .hidden


CLEANFILES =			\
  $(wildcard .*.script_ok) \
  .hidden

MAINTAINERCLEANFILES = \
  Makefile.in

EXTRA_DIST =			\
  erlrc-make-boot-script.in

install-data-hook:
	chmod 777 "$(DESTDIR)/etc/erlrc.d/nodes"
	cp "$(DESTDIR)/$(bindir)/erlrc-upgrade" "$(DESTDIR)/$(bindir)/erlrc-upgrayedd"
