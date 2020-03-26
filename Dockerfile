FROM fpco/stack-build:lts-12.26 AS gonito-build

MAINTAINER Filip Gralinski <filipg@ceti.pl>

WORKDIR /root/

RUN apt-get -y update && apt-get -y install libfcgi-dev  && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN git clone git://gonito.net/geval && cd geval && git reset --hard 599b08eb2beb207b81073acb9626696f7f0e0171
RUN git clone git://gonito.net/gonito && cd gonito && git reset --hard 4046e32f5e8e3e9bd61a880c20a37154bf30798a

WORKDIR gonito

RUN stack install && rm -rf /root/.stack

FROM ubuntu:16.04 AS gonito-run

RUN apt-get -y update && apt-get -y install libcairo2 libpq5 && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN mkdir /gonito

WORKDIR /gonito

COPY --from=gonito-build /root/.local/bin/gonito-bin ./

COPY --from=gonito-build /root/gonito/static static

COPY --from=gonito-build /root/gonito/config config

RUN apt-get -y update && apt-get -y install git && apt-get clean && rm -rf /var/lib/apt/lists/*

CMD ./gonito-bin
