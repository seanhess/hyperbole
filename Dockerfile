FROM haskell:9.8.2 AS base
WORKDIR /opt/build

RUN cabal update
RUN cabal install bytestring containers casing effectful text time string-interpolate file-embed http-api-data http-types wai warp wai-websockets network cookie string-conversions hpack websockets


FROM haskell:9.8.2 AS dependencies
WORKDIR /opt/build
COPY --from=base /root/.cache /root/.cache
COPY --from=base /root/.local /root/.local
COPY --from=base /root/.config /root/.config

# RUN apt-get update && apt-get install -y libpcre3 libpcre3-dev libcurl4-openssl-dev cron vim rsyslog
ADD ./package.yaml .
# ADD ./cabal.project .
# ADD ./docs/docgen.cabal .
# ADD ./demo/demo.cabal .
RUN hpack
RUN cabal update
RUN cabal build --only-dependencies

FROM haskell:9.8.2 AS build
WORKDIR /opt/build
COPY --from=dependencies /root/.cache /root/.cache
COPY --from=dependencies /root/.local /root/.local
COPY --from=dependencies /root/.config /root/.config
ADD ./package.yaml .
ADD ./cabal.project .
ADD ./client ./client
ADD ./test ./test
ADD ./src ./src
ADD ./demo ./demo
ADD ./docs ./docs
ADD *.md .
ADD LICENSE .
RUN hpack
RUN hpack demo
RUN hpack docs
RUN cabal build demo
RUN mkdir bin
RUN cd demo && export EXEC=$(cabal list-bin demo | tail -n1); cp "$EXEC" /opt/build/bin/demo


FROM ubuntu:24.04 AS app
WORKDIR /opt/app

RUN apt-get update
RUN apt-get install -y --no-install-recommends ca-certificates
RUN update-ca-certificates && rm -rf /var/lib/apt/lists/*

COPY --from=build /opt/build/bin/demo ./bin/demo
ADD ./client ./client
ADD ./demo/static ./demo/static

# ENV DYNAMO_LOCAL=False
ENTRYPOINT ["/opt/app/bin/demo"]
