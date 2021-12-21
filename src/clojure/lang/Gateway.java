package clojure.lang;

import clojure.lang.Compiler.LocalBinding;
import clojure.lang.Symbol;

public final class Gateway {

    public static LocalBinding localBinding(int num, Symbol sym, Symbol tag, boolean isArg) {
        return new LocalBinding(num, sym, tag, null, isArg, null);
    }
}
