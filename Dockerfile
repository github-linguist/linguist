FROM ruby:2-alpine

RUN apk --update add --virtual build_deps \
    build-base \
    libc-dev \
    linux-headers \
    cmake \
    && apk --no-cache add icu-dev libressl-dev \
    && gem install github-linguist \
    && apk del build_deps build-base libc-dev linux-headers cmake

CMD ["github-linguist"]
