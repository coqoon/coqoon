Coqoon 0.4.4.1
==============

27th March 2014

This minor release addresses a table rendering problem on Windows, adds syntax
highlighting for strings, and fixes a minor bug in the automatic indentation
code.

Coqoon 0.4.4
============

24th March 2014

This release improves the reliability of the model's tagging system, adds
automatic indentation for Coq code, and introduces a new goal viewer with
identifier highlighting.

Other significant changes
-------------------------

* Changed the behaviour of the Coqoon load path extension point

  Extensions can now take responsibility for entire blocks of the Coqoon
  abstract load path identifier space. This should make it possible to
  integrate Coqoon with tools like Coq package managers.

* Added support for bracket highlighting

  Selecting a parenthesis in the Coq editor will now automatically highlight
  its closing (or opening) counterpart.

* Improved the syntax highlighter's token detection code

  The Coq editor can now distinguish between tokens like `Program` and
  `Program.Basics`, and correctly detects and highlights (some) Coq operators.

Bug fixes
---------

* Don't try to invoke the Coq compiler for files in an invalid path
* Don't perform code folding on very short blocks of code

Coqoon 0.4.3
============

29th January 2014

This release extends the Coq model to support temporary working copies (which
are now used to provide outline and code folding support in the Coq editor)
and adds support for sending interrupts to Coq on Windows.

Other significant changes
-------------------------

* Deleted all of the old code parsing infrastructure

  The parser was once used to provide outline and code folding support, but
  the model's now taken over from it.

Bug fixes
---------

* Re-enabled support for sending interrupts in the Java editor

Coqoon 0.4.2.1
==============

14th January 2014

This minor release adds support for Coq 8.4pl3's version of the
`coqtop -ideslave` protocol.

Coqoon 0.4.2
============

20th December 2013

This release makes the Coq model better at structuring proof scripts, improves
the "Open Declaration" feature, and tidies up the Model Explorer, presenting
Coq projects in a more Java-like way. (The Model Explorer will be merged into
the normal Eclipse Project Explorer in a future release.)

Other significant changes
-------------------------

* Improved the reliability of the Coq project builder

  The builder now deletes .vo files when an attempt to rebuild them fails, and
  no longer attempts to delete output root directories (like `bin/`).

* Fixed the Kopitiam proof initialisation process

  Kopitiam has relied on the Coq project builder since v0.4.0, but there's been
  no way to add the Coq builder to an existing Java project. This release
  fixes this problem: the user will now be prompted to do this as part of the
  "Verify method" action. This change fixes issue #44.

* Expanded the Coq model's support for events

  Events are now generated whenever Coq elements are added, removed or
  modified.

Bug fixes
---------

* Made the Coq model more robust against content type lookup errors
* Added support for incomplete proof scripts to the model
* Fixed the Coq model's unhelpfully-delayed event broadcast system

Coqoon 0.4.1
============

4th December 2013

This release adds support for parallel builds and extends the Coq model with
support for retrieving and structuring proof scripts.

This release also includes a new "Open Declaration" feature modelled after that
of Java: pressing the F3 key when the cursor's over an identifier will attempt
to jump to its definition. (This is just a technology preview in this release:
in particular, it's not yet possible to jump to identifiers defined in other
files.)

Other significant changes
-------------------------

* Refactored the Makefile generation code

  In an attempt to make CoqoonMakefiles more portable, Coqoon now generates
  several Makefile fragments. This change fixes (most of) issue #38.

* Fixed a Java 6 compatibility error

  When they were created, the Coqoon core and UI plugins were given unnecessary
  dependencies on Java 7, which stopped Coqoon from working properly on Java 6;
  this release fixes these faulty dependencies.

* Improved the display of error markers and highlighted regions

  Leading whitespace is now skipped when selecting document regions. This
  change fixes issue #42.

Bug fixes
---------

* Moved more definitions from the Kopitiam plug-in manifest into Coqoon proper

Coqoon 0.4.0
============

12th November 2013

This is the first release in the 0.4.x series of what was once called
Kopitiam.

This is chiefly an infrastructural release which reorganises the code base
into several Eclipse plugins:

* the Coqoon core, implementing the <code>coqtop -ideslave</code> protocol,
Coq project support, the Coq builder and the Coq model (`dk.itu.coqoon.core`);
* the Coqoon UI, the Coq editor and its associated Eclipse bits and pieces
(`dk.itu.coqoon.ui`);
and
* Kopitiam, the Java proof environment (`dk.itu.sdg.kopitiam`).

Other significant changes
-------------------------

* Changed the way the Coq compiler is invoked

  The Coq compiler wrapper no longer attempts to write to resources in the
  Eclipse workspace. This change has no user-visible consequences in this
  version of Coqoon, but a future release will build upon this to add support
  for parallel builds.

* Added an Eclipse extension point for load path resolution

  Third-party plugins can now contribute abstract load path providers to
  Coqoon through the new `dk.itu.coqoon.core.loadpath` extension point.

Bug fixes
---------

* Fixed a synchronisation issue in the Java proof initialisation code
* Fixed an incorrect assumption in the output folder filter
* Removed the now-useless "smart compilation" option

---

Older changelog entries can be found in the
`plugins/dk.itu.sdg.kopitiam/Changelog.org` file.
