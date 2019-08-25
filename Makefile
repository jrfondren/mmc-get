FILES=package postpackage analysis lint mmcget manager
BIN=postpackage lint mmc-get
DEPS=package.m manager.m
GENEXT=d,o,mh,err,c,c_date,mh
GRADE=hlc.gc
FLAGS=-s $(GRADE) -O4 --intermodule-optimization

all:: $(BIN)

deploy:: all
	chmod 755 postpackage
	mv -f -v postpackage $(HOME)/public_html/packages/postpackage.cgi
	mv -f -v lint $(HOME)/bin/

install:: mmc-get
	mv -f -v mmc-get $(HOME)/bin/

%: %.m $(DEPS)
	mmc $(FLAGS) --make $@

mmc-get: mmcget.m $(DEPS) ioextra.m
	mmc $(FLAGS) --make mmcget && mv -fv mmcget mmc-get

clean::
	rm -rf Mercury
	rm -fv $$(for x in $(FILES); do echo $$x.{$(GENEXT)}; done)
	rm -fv $(BIN)
