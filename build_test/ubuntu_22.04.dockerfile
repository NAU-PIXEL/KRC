FROM ubuntu:22.04

RUN apt update && apt install -y gcc build-essential make csh gfortran

WORKDIR /usr/krc/

COPY Makefile /usr/krc/Makefile
COPY src/ /usr/krc/src/
COPY idl/ /usr/krc/idl/

RUN make krc
