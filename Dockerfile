FROM ruby:alpine

RUN apk --update add --virtual build_deps \
    cmake \
    && apk --no-cache add icu-dev libressl-dev \
    && gem install github-linguist \
    && apk del build_deps

CMD ["github-linguist"]
