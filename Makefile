#
# $Id$
#

VERSION = 1.1.0

TAR	= tar
RM	= /bin/rm -f
EMACS	= emacs
FLAGS   = -batch -q -no-site-file -l RMAIL-MIME-MK

PREFIX =

FILES =	README.en Makefile RMAIL-MIME-MK RMAIL-MIME-CFG RMAIL-MIME-ELS \
	*.el ChangeLog


elc:
	$(EMACS) $(FLAGS) -f compile-rmail-mime

install:	elc
	$(EMACS) $(FLAGS) -f install-rmail-mime $(PREFIX)


clean:
	-rm *.elc


tar:
	cvs commit
	sh -c 'cvs tag -RF rmail-mime-`echo $(VERSION)|sed s/\\\\./_/`; \
	cd /tmp; \
	cvs -d :pserver:anonymous@chamonix.jaist.ac.jp:/hare/cvs/root \
		export -d rmail-mime-$(VERSION) \
		-r rmail-mime-`echo $(VERSION) \
			| sed s/\\\\./_/ | sed s/\\\\./_/` rmail-mime'
	cd /tmp; $(TAR) cvzf rmail-mime-$(VERSION).tar.gz rmail-mime-$(VERSION)
	cd /tmp; $(RM) -r rmail-mime-$(VERSION)
	sed "s/VERSION/$(VERSION)/" < ftp.in > ftp
#	tar cvf ../rmail-mime-$(VERSION).tar $(FILES)
#	-cd ..; mkdir rmail-mime-$(VERSION)
#	cd ../rmail-mime-$(VERSION); $(TAR) xvf ../rmail-mime-$(VERSION).tar
#	cd ..; $(TAR) cvzf rmail-mime-$(VERSION).tar.gz rmail-mime-$(VERSION)
#	cd ..; $(RM) -r rmail-mime-$(VERSION); rm rmail-mime-$(VERSION).tar
