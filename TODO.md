TODO: Coqoon
------------

* Implement a model for Coq

  In order to be able to perform interesting operations (searching,
  refactoring, ...) on Coq code, we need a model for it, like the JDT's model
  for Java code.
  
  Thanks to notation, dependent types, and the lack of a stable object file
  format, this will be rather harder for Coq than it was for Java, and it
  probably won't be possible for the model to be quite as detailed.
  
* Refactoring

  Being able to perform refactoring -- renaming proofs, promoting nested helper
  lemmas into top-level ones, moving proofs between files, etc. -- would be
  very useful.
  
  (Eclipse has lots of built-in support for refactoring, which should make this
  fairly easy to implement.)

* "Jump to"

  In the Java editor, pressing F3 (or choosing the "Navigate -> Open
  Declaration" menu item) shows the definition of the identifier under the
  text cursor; this should also work in the Coq editor.
  
  (This becomes a bit trickier when the source isn't available. For Java,
  Eclipse shows a disassembly of the object code associated with the
  definition; what should we do for Coq object code?)

* Advanced Java-like search functionality

  The Eclipse Java editor allows the user to search for definitions,
  references, implementations, and accesses associated with an identifier; the
  Coq editor should support something like this, too.

* Extend the functionality of Coq projects

  Most of the functionality of the ICoqProject trait is either hard-coded or
  dummied out -- for example, the user can't define a project-specific Coq load
  path.
  
  (Ideally, Coq load paths would be handled in the same way as Java build
  paths.)

* Add support for interrupts on Windows

  Interrupting Coq on Windows is tricky, as Windows doesn't really support
  POSIX signals. CoqIde solves this with some native code trickery, but it's
  not clear that this would work properly in the JVM (as Java registers its own
  signal handlers).
  
  The Windows API function "GenerateConsoleCtrlEvent" is used to send emulated
  POSIX signals, but it's rather restrictive (in particular, it relies on a
  process group identifier which the JRE doesn't make generally available).

* Persist the Coq state across sessions

  Using Coq's "Write State" and "Restore State" commands should allow the user
  to save and reload a previous Coq session.
  
  (Obviously this isn't quite enough -- in particular, Coqoon's own internal
  state should also be saved somewhere.)
  
* Handle Coq commands which conflict with Coqoon specially?

  Some Coq proof script commands are deprecated inside Coqoon -- for example,
  the project configuration page should always be used instead of the "Add
  LoadPath" command, and the extraction commands should probably be wrapped in
  a wizard to avoid filling the workspace with unsynchronised resources.
  
  Should Coqoon do something when it encounters these commands? Should it warn
  the user, or perhaps even prevent the commands from being passed to coqtop?

TODO: Kopitiam
--------------

* Multiple commands in an antiquote

  Antiquotes -- the "<%" and "%>" markers used to decorate Java code -- can't
  contain more than one Coq command.
  
  (This limitation is imposed by the way that JavaEditorState keeps track of
  the "underway" and "completed" markers -- it's not currently possible to
  break an underlying Java AST node into more than one executable region.)

* Support for more fundamental Java language features

  Many fundamental Java features are missing from Charge! (particularly arrays,
  generics, exceptions and casts), and these should be implemented to make the
  supported language more Java-like.

* Automatic translation for unsupported language constructs

  It'd be tremendously useful if unsupported language constructs could be
  translated into supported ones. As an example, the Java for loop -- which
  is not supported -- could be represented as a while loop, which is.
  
  (This was supported in an earlier version of Kopitiam, but it didn't survive
  the port to the Eclipse AST.)
