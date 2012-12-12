SPHINX_DIR=/spin/$(USER)/spin/user/sphinx/src

SRC=Dequeue.ig Dequeue.mg Round.m3 Round.i3 dequeue.tmpl
ARCH_SRC=Types.i3

all:
	$(MAKE) -C../..
setup: $(SRC) $(ARCH_SRC)

$(SRC):%: $(SPHINX_DIR)/%
	cp $^ .

$(ARCH_SRC):%: $(SPHINX_DIR)/$(M3ARCH)/%
	if [ "$(M3ARCH)x" = "x" ]; then \
		echo M3ARCH not set. Call make from trans toplevel.; \
	else \
		cp $^ .; \
	fi

