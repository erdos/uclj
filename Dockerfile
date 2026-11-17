
# Stage 1: Build
FROM ghcr.io/graalvm/native-image-community:25i4 AS builder

RUN microdnf --enablerepo=ol10_codeready_builder \
        install -y curl gcc glibc-devel glibc-static zlib-devel \
    && microdnf clean all

    # Install Leiningen
RUN curl -fsSL \
    https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein \
    -o /usr/local/bin/lein \
    && chmod +x /usr/local/bin/lein

ENV LEIN_ROOT=1

WORKDIR /app
COPY . .

RUN java -version && native-image --version
RUN lein test && lein uberjar

RUN native-image \
    --no-fallback \
    -H:ReflectionConfigurationFiles=reflectconfig \
    --initialize-at-build-time \
    --enable-url-protocols=http,https \
    -jar target/uclj-*-standalone.jar \
    uclj

# Stage 2: Runtime
FROM debian:bookworm-slim

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       ca-certificates zlib1g \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /app/uclj /usr/local/bin/uclj

WORKDIR /work
ENTRYPOINT ["/usr/local/bin/uclj"]
