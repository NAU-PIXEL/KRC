FROM redhat/ubi9:9.7

RUN dnf install -y gcc make gfortran

WORKDIR /usr/krc/

COPY Makefile /usr/krc/Makefile
COPY src/ /usr/krc/src/
COPY idl/ /usr/krc/idl/

RUN make krc
