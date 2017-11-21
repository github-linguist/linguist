FROM golang:1.9.2

RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install -y curl gnupg

RUN curl -sL https://deb.nodesource.com/setup_6.x | bash -
RUN apt-get install -y nodejs
RUN npm install -g season

WORKDIR /go/src/github.com/github/linguist/tools/grammars
COPY . .

RUN go install ./cmd/grammar-compiler