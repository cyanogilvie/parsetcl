FROM ubuntu:latest
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update && apt-get install -y build-essential autoconf tcl-dev git && rm -rf /var/lib/apt/lists/*
WORKDIR /src/tdom
RUN git clone -b cyan-0.9.3.2 --recurse-submodules --shallow-submodules --single-branch --depth 1 https://github.com/RubyLane/tdom .
RUN autoconf && ./configure CFLAGS="${CFLAGS}" --enable-symbols && \
	make -j 8 all && \
	make install-binaries install-libraries
