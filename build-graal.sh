#!/usr/bin/env sh

set -euxo pipefail

lein uberjar

JARFILE=`ls target/uclj-*-standalone.jar`

$GRAALVM_HOME/bin/native-image \
    --no-fallback \
    -H:ReflectionConfigurationFiles=reflectconfig \
    --initialize-at-build-time \
    --allow-incomplete-classpath  \
    -jar $JARFILE uclj
