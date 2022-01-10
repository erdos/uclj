#!/usr/bin/env sh

set -euxo pipefail

lein uberjar

JARFILE=`ls target/uclj-*-standalone.jar`

if [ -z "$GRAALVM_HOME" ]
then
    NATIVE_IMAGE="$GRAALVM_HOME/bin/native-image"
else
    NATIVE_IMAGE=native-image
fi

if ! command -v $NATIVE_IMAGE &> /dev/null
then
    echo "native-imagee could not be found"
    exit 1
fi

$NATIVE_IMAGE \
    --no-fallback \
    -H:ReflectionConfigurationFiles=reflectconfig \
    --initialize-at-build-time \
    --allow-incomplete-classpath  \
    -jar $JARFILE uclj
