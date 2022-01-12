#!/usr/bin/env sh

set -euxo pipefail

if [ -n "${GRAALVM_HOME-}" ]; then
    NATIVE_IMAGE="$GRAALVM_HOME/bin/native-image"
else
    NATIVE_IMAGE=native-image
fi

command -v $NATIVE_IMAGE

JARFILE=`ls target/uclj*-standalone.jar`

if [ ! -f "$JARFILE" ]; then
    lein uberjar
else
    echo "Jar file already exists!"
fi

$NATIVE_IMAGE \
    --no-fallback \
    -H:ReflectionConfigurationFiles=reflectconfig \
    --initialize-at-build-time \
    --allow-incomplete-classpath  \
    -jar $JARFILE uclj
