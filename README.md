# uclj

A small, quick-starting, native Clojure interpreter.

It is built on top of the Clojure JVM runtime, but the parts that need dynamic class loading have been reimplemented in Clojure so that it could be compiled into a native application.

**Features**

- Starts quickly (it is compiled with GraalVM native-image)
- Small (≪1K SLOC)
- Out of the Box [core.async](https://github.com/clojure/core.async) support

## Usage

Download (or compile) the standalone `uclj` binary and just run it:
- call `uclj` without parameters to get a REPL
- call `uclj filename.clj` to load a file
- call `uclj '(...)'` to evaluate a Clojure expression. (It must start with a `(` character.)

### Build

You will need [Leiningen](https://leiningen.org/) to build the application. Set the `GRAALVM_HOME` environment variable and run the `build-graal.sh` script.

### Benchmarks

The author's [Advent Of Code 2021 Clojure solutions](https://github.com/erdos/advent-of-code) are used for benchmarking. The values are runtimes in _mean + standard deviation_ format in milliseconds, the smaller is the better. See `benchmark.clj` for details.

| test case | uclj | [bb v0.7.0](https://github.com/babashka/babashka) | [clojure 1.10.3](https://clojure.org/)
| --------- | ---- | ------------------------------------------------- | ---------------------------------------
| day1.clj  |          18+6          |      29+1         |       931+62
| day2.clj  |          36+23         |      30+1         |       925+34
| day3.clj  |          65+7          |      183+23       |       1024+69
| day4.clj  |          216+6          |     457+1        |       1073+52
| day5.clj  |          4599+42        |     5009+169     |       2629+226
| day6.clj  |          11+0           |     9+0          |       894+6
| day8.clj  |          105+6          |     106+0        |       1284+172
| day9.clj  |          645+3          |     661+118      |       1607+37
| day24.clj |          4639+175       |     7494+98      |       1409+13

You can see that for heavier tasks, running the `clojure` is the fastest. For lighter tasks with high number of function invocations and loops, `uclj` can be the fastest.

## License

Copyright © 2021 Janos Erdos

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
