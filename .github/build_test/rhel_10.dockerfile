FROM redhat/ubi10:10.1

RUN dnf install -y gcc make gfortran

WORKDIR /usr/krc/

COPY Makefile /usr/krc/Makefile
COPY src/ /usr/krc/src/

RUN make krc
