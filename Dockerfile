FROM fpco/stack-build:lts-12.26 AS gonito-build

MAINTAINER Filip Gralinski <filipg@ceti.pl>

WORKDIR /root/

RUN apt-get -y update && apt-get -y install libfcgi-dev && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN git clone git://gonito.net/geval && cd geval && git reset --hard a49abb560bd5e5f11f291f50176934fef5352c6c
RUN git clone git://gonito.net/gonito && cd gonito && git reset --hard 644a13cd7f4d59c07d459d299e031f3e576ea1b8

WORKDIR gonito

RUN stack install && rm -rf /root/.stack

FROM ubuntu:16.04 AS gonito-run

RUN apt-get -y update && apt-get -y install git libcairo2 libpq5 && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN mkdir /gonito

WORKDIR /gonito

COPY --from=gonito-build /root/.local/bin/gonito-bin ./

COPY --from=gonito-build /root/gonito/static static

COPY --from=gonito-build /root/gonito/config config

CMD ./gonito-bin
