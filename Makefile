#
# $Id$
#

VERSION = 0.7

TAR	= gtar
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
	tar cvf ../rmail-mime-$(VERSION).tar $(FILES)
	-cd ..; mkdir rmail-mime-$(VERSION)
	cd ../rmail-mime-$(VERSION); $(TAR) xvf ../rmail-mime-$(VERSION).tar
	cd ..; $(TAR) cvzf rmail-mime-$(VERSION).tar.gz rmail-mime-$(VERSION)
	cd ..; $(RM) -r rmail-mime-$(VERSION); rm rmail-mime-$(VERSION).tar
