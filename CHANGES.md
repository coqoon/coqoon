Coqoon 0.7.1.2
==============

29th January 2016

This minor release improves the performance and reduces the memory usage of
the PIDE editor. (To improve the performance further, use this release
together with an up-to-date version of the PIDE plugin for Coq.)

Bug fixes
---------

* Clicking annotations now correctly triggers a perspective update

Known issues
------------

* Progress annotation flickering

  The PIDE editor's progress annotations flicker noticeably on some files.

Coqoon 0.7.1.1
==============

15th January 2016

This minor release fixes some minor bugs in the new syntax highlighting code
and adds an option for disabling PIDE perspectives.

Other significant features
--------------------------

* Coq path cache

  Coqoon now caches the result of `coqtop -where` instead of running the
  command every time it needs to find the standard library.

* Better goal viewer behaviour

  When the currently-selected command has an error, the goal viewer will no
  longer be cleared.

Coqoon 0.7.1
============

18th December 2015

This release drastically improves the syntax highlighting code, improving
performance and bringing complete support for nested comments.

Bug fixes
---------

* Fatal PIDE errors are now captured and reported instead of being ignored
* Coq error summaries now record more useful information
* Errors during Coqoon shutdown should no longer cause the UI to freeze

Coqoon 0.7.0.2
==============

29th October 2015

This minor release makes a small change to the way Coqoon sends PIDE messages
that should, when used with an up-to-date version of the PIDE plugin for Coq,
greatly improve the reliability of the PIDE editor.

Coqoon 0.7.0.1
==============

26th October 2015

This minor release makes the PIDE editor clean up error markers more
aggressively and works around an occasional crashing bug in the Coq builder
triggered by running Coq in `-debug` mode.

Coqoon 0.7.0
============

21st October 2015

This release adds support for the Coq 8.5 quick compilation chain and for
compiling Coq developments with embedded OCaml plugins.

([OCaml](http://ocaml.org/) and [OcaIDE](http://www.algo-prog.info/ocaide/)
are also required to compile OCaml plugins.)

Other significant features
--------------------------

* True PIDE perspective support

  When using the PIDE editor, Coq will now only evaluate the region of the
  document shown in the editor's viewport (along with anything that region
  might depend upon).

* Local overrides for external paths

  Dependencies on external paths can now be overridden by the user without
  needing to change the original `_CoqProject` file.

* Tab completion support in the build script

  On platforms that support the `readline` library, the build script now
  supports tab completion of paths when providing values for variables.

* OCaml module dependencies

  Coqoon now recognises that sentences of the form `Declare ML Module "X".`
  introduce dependencies on OCaml libraries.

Bug fixes
---------

* Made the "Open Declaration" feature less picky about offsets
* The Python build script no longer tries to overwrite Makefiles it didn't
  create
* PIDE no longer crashes upon encountering a parenthesis followed by a
  quotation mark
* Improved the performance of the PIDE editor
* Improved the I/O performance of the Coq compiler wrapper
* Drastically improved the performance of the Coq builder's dependency
  resolution code
* Added (limited) support for sentences of the form `From X Import Y.`
* Improved the Coq builder's support for ML load paths

Coqoon 0.6.0
============

30th July 2015

This release adds support for running PIDE queries, improves the goal viewer
by showing more proof state information, and makes the "Open Declaration" and
"Reformat" features work in the PIDE editor.

Other significant features
--------------------------

* Support for PIDE entity information

  The "Open Declaration" feature now uses PIDE entity information when
  possible, making it possible to open declarations from external libraries.

* Limited support for external files

  The PIDE editor can now display files that are not associated with a Coqoon
  project, although the other services of the editor will not be available.

* Cache support in the configure script

  The Python configure script can now optionally extract variable values from
  Makefiles that it has previously generated.

* Improved bullet support in the model

  The internal model of Coq code now has full support for proof-structuring
  bullets; as a consequence, the reformatting code now handles them properly,
  too.

Coqoon 0.5.9.1
==============

1st June 2015

This exceedingly minor release bumps the version number of the embedded PIDE
snapshot to restore a missing method; the version numbers of other plugins
remain unchanged.

Coqoon 0.5.9
============

1st June 2015

This minor release adds a cap on the number of running PIDE sessions and
changes the namespace enforcement behaviour to follow that of Coq 8.5.

Other significant features
--------------------------

* Command-line options

  Extra command-line options for Coq can now be added through the preferences
  page.

Bug fixes
---------

* Delete PIDE errors even when command content stays the same (issue #94)
* Don't attempt to create PIDE progress annotations for hidden commands

Coqoon 0.5.8.2
==============

12th May 2015

This minor release restores the old way of creating PIDE sessions.

Coqoon 0.5.8.1
==============

11th May 2015

This exceedingly minor release fixes the build process for the embedded PIDE
snapshot.

Coqoon 0.5.8
============

11th May 2015

This minor release changes the way PIDE sessions are created and managed in an
attempt to reduce resource consumption and adds support for Coq 8.5beta2.

Coqoon 0.5.7
============

2nd March 2015

This minor release fixes an occasional bug that caused the PIDE editor to
create duplicate error markers and adds support for producing debugging output
when PIDE markers are created or removed.

Coqoon 0.5.6
============

27th February 2015

This minor release fixes a bug in the Python configure script triggered by
using external load path entries.

Coqoon 0.5.5
============

16th February 2015

This minor release adds support for detecting dependency cycles to the Coq
project builder.

Coqoon 0.5.4
============

13th February 2015

This minor release simplifies the process by which the builder chooses new
build candidates.

Coqoon 0.5.3
============

13th February 2015

This minor release fixes a bug that caused anonymous external dependencies to
be misresolved when a project depended on another project that had such a
dependency.

Coqoon 0.5.2
============

12th February 2015

This minor release fixes a bug that was causing Coqoon project configuration
files to be ignored by version control plug-ins and makes the debugging output
more configurable.

Coqoon 0.5.1
============

11th February 2015

This minor release restores and improves support for displaying goals in the
PIDE editor.

Coqoon 0.5.0
============

10th February 2015

This release switches the Coq editor to use PIDE by default and adds support
for external load paths to the Python configure script.

The old `-ideslave`-based editor is still present (to use
it, right-click a file and select the *Open With &rarr; Coq Editor (-ideslave,
v8.4)* menu option), but it is known not to work with Coq 8.5 and **will be
removed in a future release**.

Other significant features
--------------------------

* New build script preferences page

  Projects now have an additional preferences page for managing and updating
  the Python configure script and its metadata.

Bug fixes
---------

* Made coqdir handling in external load paths work properly again
* Acquire the right scheduling rule when configuring new inter-project
  dependencies

Coqoon 0.4.7
============

26th January 2015

This release replaces the old Makefile generation code with a Python
reimplementation of the Coqoon build system and completes the implementation
of the new load path user interface.

Coqoon 0.4.6.6
==============

11th November 2014

This minor release fixes the performance problems when using annotations in
the PIDE editor. (Accordingly, the option that controls these annotations is
now on by default.)

Coqoon 0.4.6.5
==============

11th November 2014

This *incredibly* minor release makes the annotation control option work
properly.

Coqoon 0.4.6.4
==============

10th November 2014

This minor release makes syntax highlighting for Coq code more reliable and
introduces a new option controlling annotations in the PIDE editor. (As these
annotations are known to cause serious performance problems on some platforms,
this option is off by default.)

Coqoon 0.4.6.3
==============

4th November 2014

This minor release improves the performance of PIDE processing annotations.

Bug fixes
---------

* The editor shouldn't break when opening a Coq source file not covered by the
  model (issue #85)
* Coqoon no longer waits forever when `coqtop` fails to start (issue #82)

Coqoon 0.4.6.2
==============

24th October 2014

This minor release adds support for processing annotations to the PIDE editor,
stops copying goal data onto the Eclipse console, and adds more location
information error marker to error markers.

Coqoon 0.4.6.1
==============

15th October 2014

This minor release fixes a bug that caused some compilation errors to lose
their location information, adds support for printing errors when the PIDE
editor caret moves, puts Coq-specific options in the "New" menu when using
the Coq perspective, and updates the embedded version of the PIDE client to
fix a deficiency in its Coq parser.

Coqoon 0.4.6
============

9th October 2014

This release introduces a new user interface for configuring project load
paths, and adds tentative support for using the PIDE protocol to interact
with Coq. (This protocol is not yet supported by any released version of Coq.)

Other significant changes
-------------------------

* Scala library version bump

  Coqoon now depends on Scala 2.11.

Coqoon 0.4.5.2
==============

12th August 2014

This minor release fixes a bug that could cause syntax highlighting and code
folding updates to be sent to the wrong editor, which could potentially lead
to mysterious crashes in background threads.

Coqoon 0.4.5.1
==============

8th August 2014

This minor release fixes a Scala 2.11 compatibility problem and a bug in the
project importer, and adds a few extra sanity checks to address issues in
the automatic indentation and Coq model package fragment code.

Coqoon 0.4.5
============

30th June 2014

This release dramatically improves syntax highlighting, adds support for block
reindentation of Coq code, and (finally!) allows multiple Coq sentences to
appear in a Kopitiam antiquote region.

Other significant changes
-------------------------

* Indentation preferences

  Coqoon's indentation behaviour is now _very slightly_ configurable: the
  number of spaces per level of indentation can be changed and the automatic
  indentation can be switched off entirely.

* Proper document partitioning

  Coq proof scripts are now partitioned into several different content types:
  one for Coq, one for strings, and one for comments. Not only does this
  make the syntax highlighting much more robust, it also makes it possible to
  switch off code assistants that don't make sense for a given content type:
  for example, autocompletion no longer does anything inside a comment.

* Experimental namespace enforcement

  Coqoon normally includes a project's files and dependencies using `Add Rec
  LoadPath` (or the equivalent `-R` command-line option), which clutters up
  the global namespace. This behaviour can now optionally be replaced with
  behaviour akin to that of Scala packages.

  (Note that this option is experimental, and is quite likely to change
  between versions&mdash;or to go away entirely.)

* Debugging options

  Coqoon can now dump its communications with `coqtop` processes to standard
  output.

* Cleaner Coq model implementation

  Coq model objects can now be created without any backing objects in the
  workspace.

* Syntax highlighting for option keywords and option names

  Coqoon now recognises the "Set" and "Unset" keywords, and displays Coq
  option names in a different colour. This change resolves issue #42.

Bug fixes
---------

* Correctly handle comment tokens that begin with multiple asterisks (issue
  #54)
* Pressing Enter after a "Qed." shouldn't necessarily retract it (issue #57)
* Deleting whitespace between two sentences should (potentially) cause the
  first sentence to be retracted
* Initialise the Coqoon preferences correctly even when the Kopitiam plugin
  isn't present

Coqoon 0.4.4.3
==============

9th April 2014

This minor release restores Windows compatibility by adding back some files
that had been accidentally left out of the build process.

Coqoon 0.4.4.2
==============

2nd April 2014

This minor release replaces the goal viewer's table view with a more portable
text view, and fixes a dependency resolution bug and some peculiar
autocompletion behaviour.

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
