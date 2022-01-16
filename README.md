# uclj

A small, quick-starting, native Clojure interpreter.

It is built on top of the Clojure JVM runtime, but the parts that need dynamic class loading have been reimplemented in Clojure so that it could be compiled into a native application.

**Features**

- Starts quickly (it is compiled with GraalVM native-image)
- Small (≪1K SLOC)
- Out of the Box [core.async](https://github.com/clojure/core.async) support and also [many other core libraries](https://github.com/erdos/uclj/blob/master/src/uclj/core.clj#L10)

## Usage

Download the binary from the [Release page](https://github.com/erdos/uclj/releases) and run the `uclj` command:
- call `uclj` without parameters to get a REPL
- call `uclj filename.clj` to load a file
- call `uclj filename.clj --test` to load a file and then run all test cases in it
- call `uclj '(...)'` to evaluate a Clojure expression. (It must start with a `(` character.)

### Build

You can also build the binary yourself. You will need [Leiningen](https://leiningen.org/) and [GraalVM](https://www.graalvm.org/downloads/) to build the application. Set the `GRAALVM_HOME` environment variable and run the `build-graal.sh` script.

### Benchmarks

The author's [Advent Of Code 2021 Clojure solutions](https://github.com/erdos/advent-of-code) are used for benchmarking. The values are runtimes in _mean + standard deviation_ format in milliseconds, the smaller is the better. See `benchmark.clj` for details.

| test case | uclj | [bb v0.7.3](https://github.com/babashka/babashka) | [clojure 1.10.3](https://clojure.org/)
| --------- | ---- | ------------------------------------------------- | ---------------------------------------
| day1.clj   | 22+1       | 30+1       | 913+40
| day2.clj   | 25+1       | 32+1       | 936+16
| day3.clj   | 66+2       | 180+7      | 1032+24
| day4.clj   | 184+5      | 515+15     | 1062+40
| day5.clj   | 4675+26    | 4823+15    | 2385+63
| day6.clj   | 14+1       | 9+0        | 893+48
| day7.clj   | 25397+91   | 24825+477  | 12172+151
| day8.clj   | 82+2       | 106+1      | 1064+45
| day9.clj   | 405+12     | 503+14     | 1131+43
| day11.clj  | 515+15     | 1181+43    | 1229+35
| day17.clj  | 3861+80    | 5152+143   | 1167+30
| day19.clj  | 2460+37    | 5761+145   | 2762+53
| day24.clj  | 1535+4     | 7266+163   | 1491+36

For light tasks with high number of function invocations and loops, `uclj` can be the fastest. For heavier tasks, running the `clojure` command is still winning.

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
