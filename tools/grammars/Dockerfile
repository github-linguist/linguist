FROM golang:1.9.2

RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install -y curl gnupg

RUN curl -sL https://deb.nodesource.com/setup_6.x | bash -
RUN apt-get install -y nodejs
RUN npm install -g season

RUN apt-get install -y cmake
RUN cd /tmp && git clone https://github.com/vmg/pcre
RUN mkdir -p /tmp/pcre/build && cd /tmp/pcre/build && \
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
    make && make install
RUN rm -rf /tmp/pcre

RUN go get -u github.com/golang/dep/cmd/dep
WORKDIR /go/src/github.com/github/linguist/tools/grammars
COPY . .

RUN dep ensure
RUN go install ./cmd/grammar-compiler

ENTRYPOINT ["grammar-compiler"]
