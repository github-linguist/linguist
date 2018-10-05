FROM ubuntu:16.04

RUN apt-get update && \
    apt-get install -y ruby-full \
     build-essential \
    libicu-dev \
    zlibc \
    zlib1g \
    zlib1g-dev \
    cmake \
    pkg-config \
    libcurl4-openssl-dev \
    libssl-dev \
    git

RUN ln -s /usr/bin/make /usr/bin/gmake

RUN gem install bundler

ENV LANG=C.UTF-8

WORKDIR /data

CMD tail -f /dev/null
