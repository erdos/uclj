# uclj

A small, quick-starting, native Clojure interpreter.

It is built on top of the Clojure JVM runtime, but the parts that need dynamic class loading have been reimplemented in Clojure so that it could be compiled into a native application.

**Features**

- Starts quickly (it is compiled with GraalVM native-image)
- Small (<1K SLOC)
- Out of the Box [core.async](https://github.com/clojure/core.async) support and also [many other core libraries](https://github.com/erdos/uclj/blob/master/src/uclj/core.clj#L10)

## Usage

Download the binary from the [Release page](https://github.com/erdos/uclj/releases) and run the `uclj` command:
- call `uclj` without parameters to get a REPL
- call `uclj filename.clj` to load a file
- call `uclj filename.clj --test` to load a file and then run all test cases in it
- call `uclj '(...)'` to evaluate a Clojure expression. (It must start with a `(` character.)

### Build

You can also build the binary yourself. You will need [Leiningen](https://leiningen.org/) and [GraalVM](https://www.graalvm.org/downloads/) to build the application. Set the `GRAALVM_HOME` environment variable and run the `build-graal.sh` script.

## License

Copyright © 2022 Janos Erdos

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
