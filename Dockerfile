FROM ruby:2-alpine3.13

RUN apk --update add --virtual build_deps \
    build-base \
    libc-dev \
    linux-headers \
    cmake \
    && apk --no-cache add icu-dev openssl-dev \
    && gem install github-linguist \
    && apk del build_deps build-base libc-dev linux-headers cmake

RUN addgroup --gid 1024 linguist && adduser -D -G linguist linguist
USER linguist

CMD ["github-linguist"]
