FROM fpco/stack-build:lts-12.26 AS gonito-build

MAINTAINER Filip Gralinski <filipg@ceti.pl>

WORKDIR /root/

RUN apt-get -y update && apt-get -y install libfcgi-dev && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN git clone git://gonito.net/geval && cd geval && git reset --hard 51c29aabf6c5bdc8fb5ab7a05831b9355a66bcb4
RUN git clone git://gonito.net/gonito && cd gonito && git reset --hard c15dd3080403363cddab4aacb305f448133b4814

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
