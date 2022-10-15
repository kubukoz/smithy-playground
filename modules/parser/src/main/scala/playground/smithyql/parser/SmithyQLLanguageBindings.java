package playground.smithyql;

import com.sun.jna.Library;
import com.sun.jna.NativeLibrary;
import com.sun.jna.Native;
import org.polyvariant.treesitter4s.bindings.kernel.Language;

public class SmithyQLLanguageBindings {

	private static interface Bindings extends Library {
		Language tree_sitter_smithyql();
	}

	private static final Bindings LIBRARY = Language.loadLanguageLibrary("smithyql", Bindings.class);

	public static final Language SmithyQL = LIBRARY.tree_sitter_smithyql();

}
