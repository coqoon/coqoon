#!/usr/bin/env python
# coding=utf-8

# configure.coqoon, a configure script for Coqoon projects
# A component of Coqoon, an integrated development environment for Coq proofs
# Copyright © 2014, 2015 Alexander Faithfull
#
# This script is free software; its author grants unlimited permission to use,
# copy, modify and/or redistribute it.

# Manipulating this project using Coqoon may cause this file to be overwritten
# without warning: any local changes you may have made will not be preserved.

_configure_coqoon_version = 0

import re, io, os, shlex, codecs

def striplist(l):
	return map(lambda s: s.rstrip("/"), l)

# This utility class is modelled loosely upon org.eclipse.core.runtime.Path,
# although is nowhere near as complicated
class Path:
	def __init__(self, i = None):
		if i != None:
			self._bits = striplist(i.split("/"))
		else:
			self._bits = []

	def bit(self, i):
		return self._bits[i]
	def head(self):
		return self._bits[0]
	def tail(self):
		return self.drop_first(1)

	def first(self):
		return self.head()
	def last(self):
		return self._bits[len(self) - 1]

	def drop_first(self, i):
		p = Path()
		list.extend(p._bits, self._bits[i:])
		return p
	def drop_last(self, i):
		p = Path()
		list.extend(p._bits, self._bits[:i])
		return p

	def replace(self, i, s):
		p = Path()
		list.extend(p._bits, self._bits)
		p._bits[i] = s.rstrip("/")
		return p

	def __len__(self):
		return len(self._bits)

	def append(self, i):
		if len(i) != 0:
			p = Path()
			list.extend(p._bits, self._bits)
			list.append(p._bits, i.rstrip("/"))
			return p
		else:
			return self
	def append_path(self, i):
		if len(i._bits) != 0:
			p = Path()
			list.extend(p._bits, self._bits)
			list.extend(p._bits, i._bits)
			return p
		else:
			return self

	# Convenience file operations
	def open(self, mode = "r", encoding = "utf_8"):
		return io.open(str(self), mode = mode, encoding = encoding)
	def utime(times):
		os.utime(str(self), times)

	def __str__(self):
		return "/".join(self._bits)

def load_coq_project_configuration(path):
	configuration = []
	try:
		with io.open(path, mode = "r", encoding = "utf_8") as file:
			for line in file:
				if line.startswith("KOPITIAM_"):
					split_line = shlex.split(line)
					list.append(configuration,
					            shlex.split(split_line[2]))
	except IOError:
		# If _CoqProject is missing, then use Coqoon's default settings
		configuration = [["SourceLoadPath", "src"],
		                 ["DefaultOutput", "bin"],
		                 ["AbstractLoadPath",
		                  "dk.itu.sdg.kopitiam/lp/coq/8.4"]]
	return configuration

# Read this project's configuration
configuration = load_coq_project_configuration("_CoqProject")

# Find its default output directory
default_output = None
for i in configuration:
	if i[0] == "DefaultOutput":
		default_output = i[1]
		break
if default_output == None:
	default_output = "bin"

# Find all source directories and their corresponding output directories
source_directories = []
for i in configuration:
	if i[0] == "SourceLoadPath":
		entry = None
		try:
			entry = (i[1], i[2])
		except:
			entry = (i[1], default_output)
		list.append(source_directories, entry)

def source_to_binary(curfile, srcroot, binroot):
	p = binroot.append_path(curfile.drop_first(len(srcroot)))
	return p.replace(len(p) - 1, (p.bit(len(p) - 1)) + "o")

def extract_dependency_identifiers(f):
	identifiers = []
	for line in iter(f.readline, ""):
		for (_, ids) in re.findall("(?s)Require\\s+(Import\\s+|Export\\s+|)(.*?)\\s*\\.[$\\s]", line, 0):
			# Using shlex.split here is /technically/ cheating, but
			# it means we can handle both quoted identifiers and
			# multiple identifiers with the same code
			list.extend(identifiers, shlex.split(ids))
	return identifiers

# Populate the dependency map with the basics: .vo files depend on their source
deps = {}
to_be_resolved = {}
for srcdir, bindir in source_directories:
	srcroot = Path(srcdir)
	binroot = Path(bindir)
	for current, dirs, files in os.walk(srcdir):
		curpath = Path(current)
		for sf_ in filter(lambda f: f.endswith(".v"), files):
			sf = curpath.append(sf_)
			bf = source_to_binary(sf, srcroot, binroot)
			deps[str(bf)] = [str(sf)]

			# While we're here, stash away the identifiers of the
			# dependencies of this source file
			ids = None
			with sf.open() as file:
				ids = extract_dependency_identifiers(file)
			to_be_resolved[(str(sf), str(bf))] = ids

def expand_load_path(configuration):
	# XXX: as the name of this function suggests, it should actually expand
	# the load path
	load_path = [("", "src"), ("", "bin"),
	             ("Coq", "/usr/lib/coq/theories")]
	return load_path

def try_wrap(f):
	try:
		f
		return true
	except:
		return false

complete_load_path = expand_load_path(configuration)

# Now that we know the names of all the .vo files we're going to create, we
# can use those -- along with the Coq load path -- to calculate the rest of the
# dependencies
for (sf, bf), identifiers in to_be_resolved.iteritems():
	for ident in identifiers:
		(libdir, _, libname) = ident.rpartition(".")
		for coqdir, location in complete_load_path:
			adjusted = libdir if not libdir.startswith(coqdir) \
				else libdir[len(coqdir):]
			p = Path(location)
			for i in adjusted.split("."):
				p = p.append(i)
			p = p.append(libname + ".vo")
			success = False
			try:
				os.stat(str(p))
				success = True
			except:
				success = str(p) in deps
			if success:
				list.append(deps[bf], str(p))

print deps

try:
	from socket import gethostname
	from email.Utils import formatdate
	with io.open("Makefile", mode = "w", encoding = "utf_8") as file:
		file.write(u"""\
# Generated by configure.coqoon v%d on "%s"
# at %s.
#
# This Makefile was automatically generated; any local changes you may make
# will not be preserved when it is next regenerated.

COQC = coqc
COQFLAGS = -noglob
override _COQCMD = \\
	mkdir -p "`dirname "$@"`" && $(COQC) $(COQFLAGS) "$<" && mv "$<o" "$@"

""" % (_configure_coqoon_version, gethostname(), formatdate(localtime = True)))

		# Coqoon itself actually passes -R options instead of -I ones,
		# but this works too (and we've already generated these paths
		# for the dependency resolver)
		for coqdir, location in complete_load_path:
			file.write(u"""\
override COQFLAGS += -I "%s" -as "%s"
""" % (location, coqdir))

		for srcdir, bindir in source_directories:
			file.write(u"""\
%s/%%.vo: %s/%%.v
	$(_COQCMD)

""" % (bindir, srcdir))

		file.write(u"""\
OBJECTS = \\
	%s

all: $(OBJECTS)
clean:
	rm -f $(OBJECTS)

""" % " \\\n\t".join([b for b, _ in deps.iteritems()]))

		for f, d in deps.iteritems():
			file.write(u"%s: %s\n" % (f, " ".join(d)))
except IOError as e:
	print e
	pass
