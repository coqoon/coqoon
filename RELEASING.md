# Release process

Coqoon's release process is convoluted, and it's been a while since any part of
it was tidied up. I apologise in advance for what you're about to read!

## Build the code...

This should happen more or less automatically when all of the plug-ins (and
their dependencies) have been loaded into the Eclipse workspace.

### (... with an ancient version of Eclipse...)

As of December 2018, I still build Coqoon using Eclipse 4.2, released in
2013(!) I started doing this because much of the 4.x series had _awful_
performance, and it kept getting worse with each new release, so I wanted to
make sure that one version of Coqoon would work with everything that followed
4.2, in the hope that _some_ versions would perform acceptably...

(Eclipse 4.7 and 4.8 seem to be a step in the right direction,
performance-wise, so this precaution might no longer be necessary.)

## ... update the manifests, features and changelog...

For release purposes, Coqoon's plug-ins are grouped together into _features_.
The Coqoon ecosystem has a few of these:

* `dk.itu.coqoon.feature`, which bundles together the Coqoon core, UI, and
  PIDE integration;
* `dk.itu.coqoon.ocaide.feature`, which provides OcaIDE support; and
* `dk.itu.coqoon.opam.feature` (in the separate `coqoon/coqoon-opam`
  repository), which provides integration with OPAM.

If a change has been made to any of these -- or if one of their dependencies
(for example, the Coqoon core) has changed incompatibly -- then they should get
a new version number and a new release.

(Coqoon plug-in versions and feature versions should generally be kept in
sync -- feature `0.8.4`, for example, contains versions `0.8.4` of the Coqoon
core, UI, and OcaIDE integration plug-ins.)

Here's the result of `git show --stat v0.8.1`, showing the minimal set of files
that should be updated to make a release:

```
 CHANGES.md                                      | 36 +++++++++++++++++++++++++
 features/dk.itu.coqoon.feature/feature.xml      |  2 +-
 features/update/site.xml                        |  6 +++++
 plugins/dk.itu.coqoon.core/META-INF/MANIFEST.MF |  2 +-
 plugins/dk.itu.coqoon.ui/META-INF/MANIFEST.MF   |  2 +-
 5 files changed, 45 insertions(+), 3 deletions(-)
```

(`features/update/site.xml` is the update site, which will be updated in the
next step.)

## ... update and build the update site...

Open the update site, add the new features to its `coqoon` folder, and click
`Build All`.

## ... generate the plug-ins separately...

The version of Scala IDE that I use with my ancient version of Eclipse --
`3.0.4.v-2_11-201407232043-c46f499` -- has an odd bug: it doesn't appear to
hook properly into the code that generates Eclipse update sites, so the sites
get strange versions of the Coqoon plug-ins that contain source code but no
`.class` files (and so don't actually work).

To work around this bug, run the `plugins/make-jars.sh` script _after_ building
the update site. This regenerates the plug-ins so that they include both source
_and_ binary files; move the resulting plugins into the
`features/update/plugins` directory, overwriting the broken ones.

(At present, the `plugins/make-jars.sh` script depends on the Debian package
`dctrl-tools`, which it uses to parse Java `META-INF/MANIFEST.MF` files.)

## (... check that it works...)

It's a good idea to have an installation of Coqoon which is configured to look
for the update site on your local filesystem to make sure that nothing's gone
wrong with this convoluted process (and to make sure that the remote update
site can always be used as a backup, in the highly likely case that Eclipse
does something weird to one of the update site files).

## ... commit, tag and push...

Commit the changes to the features, manifests, update site and changelog, tag
the commit with the new version number (prefixed with a `v`), and push it to
GitHub.

## ... and upload the lot to the update site

The Coqoon update site lives at `https://itu.dk/research/tomeso/coqoon/e42/`;
this corresponds to `/import/www/research/tomeso/coqoon/e42` on `ssh.itu.dk`.
Copy the contents of the `features/update/` folder here, overwriting
`artifacts.jar`, `content.jar`, and `site.xml` and merging the `features/` and
`plugins/` folders without overwriting what's already there.
