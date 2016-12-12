FROM ubuntu:14.04

MAINTAINER Wesley Hales <wesleyhales@gmail.com>

# Install.
RUN \
  sed -i 's/# \(.*multiverse$\)/\1/g' /etc/apt/sources.list && \
  apt-get update && \
  apt-get -y upgrade && \
  apt-get install -y build-essential && \
  apt-get install -y software-properties-common && \
  apt-get install -y byobu curl git htop man unzip vim wget && \
  rm -rf /var/lib/apt/lists/*

# Set environment variables.
ENV HOME /root

# Define working directory.
WORKDIR /root

# Install Java.
RUN \
  echo oracle-java7-installer shared/accepted-oracle-license-v1-1 select true | debconf-set-selections && \
  add-apt-repository -y ppa:webupd8team/java && \
  apt-get update && \
  apt-get install -y oracle-java7-installer && \
  rm -rf /var/lib/apt/lists/* \
  echo "done"

# Install Phantom2 build requirements    (Won't build on systems < 2GB ram)
RUN \
  sudo apt-get update && apt-get -y install g++ flex bison gperf ruby perl \
  libsqlite3-dev libfontconfig1-dev libicu-dev libfreetype6 libssl-dev libjpeg-dev libqt5webkit5-dev

#####################################build latest phantom
######################################+++++ only do this in dev when needed

#RUN rm -rf phantomjs

#RUN git clone git://github.com/ariya/phantomjs.git

#RUN cd /root/phantomjs/ && ./build.sh --confirm

#RUN ln -s /root/phantomjs/bin/phantomjs /usr/bin/phantomjs
######################################+++++ END only do this in dev when needed

######################################+++++ comment out when building new version of phantomjs
ADD phantomjs /root/phantomjs

RUN ln -s /root/phantomjs /usr/bin/phantomjs
######################################+++++ END comment out when building new version of phantomjs

RUN git clone git://github.com/wesleyhales/speedgun.git

#RUN mkdir /root/speedgun/core/reports

#VOLUME ["/root/speedgun/core/reports"]

RUN cd speedgun/core && phantomjs --ssl-protocol=any --ignore-ssl-errors=yes speedgun.js http://www.google.com performance csv

RUN cd /root && wget https://dl.dropboxusercontent.com/u/12278845/server.tar

RUN cd /root && tar -xvf server.tar

#RUN echo "cd /root/jboss-as-7.1.1.Final-fluxui/ && ./bin/standalone.sh --server-config=standalone-full.xml -b 0.0.0.0" >> /root/.bashrc

# install maven
RUN sudo apt-get update && apt-get install -y maven

ADD src /root/src
ADD pom.xml /root/pom.xml
RUN mvn clean install

#RUN cp -rf /root/target/speedgun.war /root/jboss-as-7.1.1.Final-fluxui/standalone/deployments/

RUN ln -s /root/target/speedgun /root/jboss-as-7.1.1.Final-fluxui/standalone/deployments/speedgun.war

RUN touch /root/jboss-as-7.1.1.Final-fluxui/standalone/deployments/speedgun.war.dodeploy

# Cleanup old JMS queue
RUN rm -rf /root/jboss-as-7.1.1.Final-fluxui/standalone/tmp/ /root/jboss-as-7.1.1.Final-fluxui/standalone/data/*

RUN mkdir /root/jboss-as-7.1.1.Final-fluxui/speedgun
RUN cd /root/jboss-as-7.1.1.Final-fluxui/speedgun && curl -O https://raw.githubusercontent.com/wesleyhales/speedgun/master/core/speedgun.js
RUN cd /root/jboss-as-7.1.1.Final-fluxui/speedgun && curl -O https://raw.githubusercontent.com/wesleyhales/speedgun/master/core/config.json

COPY server-entrypoint.sh /

ENTRYPOINT ["/server-entrypoint.sh"]

RUN apt-get install -y postgresql-client

COPY speedgun.sql /

EXPOSE 3306 8080 8443

#CMD ["postgres"]