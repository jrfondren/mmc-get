FILES=package postpackage analysis lint
BIN=postpackage lint
GENEXT=d,o,mh,err,c,c_date,mh
GRADE=hlc.gc
FLAGS=-s $(GRADE) -O4 --intermodule-optimization

all:: $(BIN)

deploy:: all
	chmod 755 postpackage
	mv -f -v postpackage $(HOME)/public_html/packages/postpackage.cgi
	mv -f -v lint $(HOME)/bin/

%: %.m
	mmc $(FLAGS) --make $@

clean::
	rm -rf Mercury
	rm -fv $$(for x in $(FILES); do echo $$x.{$(GENEXT)}; done)
	rm -fv $(BIN)
