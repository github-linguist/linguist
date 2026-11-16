FROM ruby:3.4-alpine

# The release tag (e.g. v9.6.0). Pinning the gem here is what makes the published image
# match the release it is tagged with: with a bare `gem install github-linguist` the RUN
# instruction text never changes, so the registry build cache replays the same layer on
# every release. The `#v` strips the tag's leading "v" for RubyGems.
ARG LINGUIST_VERSION=

RUN apk --update add --virtual build_deps \
    build-base \
    libc-dev \
    cmake \
    zlib-dev \
    && apk add icu-dev openssl-dev \
    && gem install github-linguist ${LINGUIST_VERSION:+-v ${LINGUIST_VERSION#v}} \
    && apk del build_deps \
	&& rm /var/cache/apk/*

CMD ["github-linguist"]
