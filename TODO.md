TODO: Coqoon
------------

* Refactoring

  Being able to perform refactoring -- renaming proofs, promoting nested helper
  lemmas into top-level ones, moving proofs between files, etc. -- would be
  very useful.
  
  (Eclipse has lots of built-in support for refactoring, which should make this
  fairly easy to implement.)

* Sourceless declaration handling

  How should Coqoon implement the "Open Declaration" action in the absence
  of source code? (For Java, Eclipse shows a disassembly of the object code
  associated with the definition; this doesn't seem like a good idea...)

* Advanced Java-like search functionality

  The Eclipse Java editor allows the user to search for definitions,
  references, implementations, and accesses associated with an identifier; the
  Coq editor should support something like this, too.

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
