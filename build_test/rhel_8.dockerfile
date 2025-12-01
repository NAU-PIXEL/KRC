FROM redhat/ubi8:8.10

RUN dnf install -y gcc make gcc-gfortran

WORKDIR /usr/krc/

COPY Makefile /usr/krc/Makefile
COPY src/ /usr/krc/src/
COPY idl/ /usr/krc/idl/

RUN make krc
