FROM debian:bullseye

RUN apt-get update && apt-get install -y systemd systemd-sysv && apt-get clean
RUN systemctl mask systemd-logind systemd-udevd

RUN apt-get install bash-completion
RUN echo "source /usr/share/bash-completion/bash_completion" >> /root/.bashrc

CMD ["/lib/systemd/systemd"]