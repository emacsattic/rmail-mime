#
# Makefile for RMAIL-MIME.
#

VERSION = 1.13.0

TAR	= tar
RM	= /bin/rm -f
EMACS	= emacs
XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -l RMAIL-MIME-MK

PREFIX = NONE
LISPDIR = NONE
PACKAGEDIR = NONE
VERSION_SPECIFIC_LISPDIR = NONE

FILES =	README.en Makefile RMAIL-MIME-MK RMAIL-MIME-CFG RMAIL-MIME-ELS \
	*.el ChangeLog


elc:
	$(EMACS) $(FLAGS) -f compile-rmail-mime $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)

install:	elc
	$(EMACS) $(FLAGS) -f install-rmail-mime $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)


package:
	$(XEMACS) $(FLAGS) -f compile-rmail-mime-package $(PACKAGEDIR)

install-package:	package
	$(XEMACS) $(FLAGS) -f install-rmail-mime-package $(PACKAGEDIR)


clean:
	-rm *.elc


tar:
	cvs commit
	sh -c 'cvs tag -RF rmail-mime-`echo $(VERSION) \
			| sed s/\\\\./_/ | sed s/\\\\./_/`; \
	cd /tmp; \
	cvs -d :pserver:anonymous@cvs.m17n.org:/cvs/root \
		export -d rmail-mime-$(VERSION) \
		-r rmail-mime-`echo $(VERSION) \
			| sed s/\\\\./_/ | sed s/\\\\./_/` rmail-mime'
	$(RM) /tmp/rmail-mime-$(VERSION)/ftp.in
	cd /tmp; $(TAR) cvzf rmail-mime-$(VERSION).tar.gz rmail-mime-$(VERSION)
	cd /tmp; $(RM) -r rmail-mime-$(VERSION)
	sed "s/VERSION/$(VERSION)/" < ftp.in > ftp
#	tar cvf ../rmail-mime-$(VERSION).tar $(FILES)
#	-cd ..; mkdir rmail-mime-$(VERSION)
#	cd ../rmail-mime-$(VERSION); $(TAR) xvf ../rmail-mime-$(VERSION).tar
#	cd ..; $(TAR) cvzf rmail-mime-$(VERSION).tar.gz rmail-mime-$(VERSION)
#	cd ..; $(RM) -r rmail-mime-$(VERSION); rm rmail-mime-$(VERSION).tar
