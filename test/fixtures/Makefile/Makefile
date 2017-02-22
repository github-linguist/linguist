SUBDIRS:=components test
.PHONY: ${SUBDIRS} clean
all:${SUBDIRS}
${SUBDIRS}:
	${MAKE} -C $@ all
