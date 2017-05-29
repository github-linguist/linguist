#!/bin/env/bash

set -e

samples=$(find samples/*)

for sample in $samples; do
  echo "=================="
  echo $sample
  ruby method.rb $sample | grep "with strategy"
done

