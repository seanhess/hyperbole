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
# ADD ./examples/examples.cabal .
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
ADD ./examples ./examples
ADD ./docs ./docs
ADD *.md .
ADD LICENSE .
RUN hpack
RUN hpack examples
RUN hpack docs
RUN cabal build examples
RUN mkdir bin
RUN cd examples && export EXEC=$(cabal list-bin examples); cp $EXEC /opt/build/bin/examples


FROM debian:10 AS app
WORKDIR /opt/app

COPY --from=build /opt/build/bin/examples ./examples
ADD ./client ./client
ADD ./examples/static ./examples/static

# ENV DYNAMO_LOCAL=False
ENTRYPOINT ["/opt/app/examples"]
