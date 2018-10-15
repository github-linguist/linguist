# Dockerfile to quick language detection from command line.
# It should help Windows users because it's easier to get dependencies on Linux ;)
FROM ubuntu

RUN apt-get -y update && apt-get install -y ruby-full cmake pkg-config libicu-dev zlib1g-dev libcurl4-openssl-dev libssl-dev
RUN gem install github-linguist

# You can mount your project to test to i.e. `/files` directory:
# docker run -v C:\path\to\test\project:/files name-of-linguist-container
CMD cd files && linguist
