Coqoon 0.4.1
============

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
