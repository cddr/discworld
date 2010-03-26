
VERSION=$(shell scripts/VERSION-GEN)
TARNAME=discworld-$(VERSION)

discworld.asd: discworld.asd.in
	sed -e 's/@@VERSION@@/$(VERSION)/g' < $< > $@+
	mv $@+ $@


dist: discworld.asd
	git-archive --format=tar \
		    --prefix=$(TARNAME)/ \
		    HEAD^{tree} >$(TARNAME).tar
	@mkdir -p $(TARNAME)
	@cp discworld.asd $(TARNAME)
	tar -rf $(TARNAME).tar \
	  $(TARNAME)/discworld.asd
	rm -r $(TARNAME)
	gzip -f -9 $(TARNAME).tar
