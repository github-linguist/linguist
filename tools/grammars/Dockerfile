FROM golang:1.22.1

WORKDIR /go/src/github.com/github/linguist/tools/grammars

RUN curl -sL https://deb.nodesource.com/setup_14.x | bash - && \
	apt-get update && \
	apt-get install -y nodejs cmake && \
	npm install -g season && \
	cd /tmp && git clone https://github.com/vmg/pcre && \
	mkdir -p /tmp/pcre/build && cd /tmp/pcre/build && \
	cmake .. \
		-DPCRE_SUPPORT_JIT=ON \
		-DPCRE_SUPPORT_UTF=ON \
		-DPCRE_SUPPORT_UNICODE_PROPERTIES=ON \
		-DBUILD_SHARED_LIBS=OFF \
		-DCMAKE_C_FLAGS="-fPIC $(EXTRA_PCRE_CFLAGS)" \
		-DCMAKE_BUILD_TYPE=RelWithDebInfo \
		-DPCRE_BUILD_PCRECPP=OFF \
		-DPCRE_BUILD_PCREGREP=OFF \
		-DPCRE_BUILD_TESTS=OFF \
		-G "Unix Makefiles" && \
    make && make install && \
	rm -rf /tmp/pcre && \
	rm -rf /var/lib/apt/lists/*

COPY . .
RUN go install ./cmd/grammar-compiler

ENTRYPOINT ["grammar-compiler"]
