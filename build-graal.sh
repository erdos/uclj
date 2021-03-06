#!/usr/bin/env sh

set -euxo pipefail

if [ -n "${GRAALVM_HOME-}" ]; then
    NATIVE_IMAGE="$GRAALVM_HOME/bin/native-image"
else
    NATIVE_IMAGE=native-image
fi

command -v $NATIVE_IMAGE

$NATIVE_IMAGE --version

JARFILE=`ls target/uclj*-standalone.jar`

if [ ! -f "$JARFILE" ]; then
    lein uberjar
else
    echo "Jar file already exists!"
fi

$NATIVE_IMAGE \
    --no-fallback \
    --no-server \
    --verbose \
    --native-image-info \
    -H:ReflectionConfigurationFiles=reflectconfig \
    -H:+ReportExceptionStackTraces \
    --initialize-at-build-time \
    --allow-incomplete-classpath  \
    --enable-url-protocols=http,https \
    -jar $JARFILE \
    uclj

chmod a+x uclj
