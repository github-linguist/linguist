#!/bin/sh
sudo apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends libicu-dev

script/bootstrap && bundle exec rake samples
