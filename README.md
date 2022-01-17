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
| test case  | uclj       | bb         | clojure
| day1.clj   | 20+1       | 26+1       | 873+18
| day2.clj   | 36+1       | 34+1       | 913+9
| day3.clj   | 64+2       | 175+7      | 1016+13
| day4.clj   | 184+6      | 482+16     | 1018+19
| day5.clj   | 4514+37    | 4616+12    | 2401+33
| day6.clj   | 12+1       | 8+1        | 885+38
| day7.clj   | 1489+75    | 3439+106   | 1128+24
| day8.clj   | 80+1       | 105+4      | 1095+55
| day9.clj   | 390+9      | 510+19     | 1106+10
| day11.clj  | 485+11     | 1201+42    | 1211+27
| day17.clj  | 4000+194   | 5147+137   | 1158+28
| day19.clj  | 2565+103   | 5726+169   | 2787+88
| day24.clj  | 1541+12    | 7414+227   | 1499+55

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
