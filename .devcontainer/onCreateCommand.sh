#!/bin/sh
./script/bootstrap
git remote add linguist https://github.com/github-linguist/linguist
git fetch linguist v2.0.0:v2.0.0 test/attributes:test/attributes test/master:test/master
