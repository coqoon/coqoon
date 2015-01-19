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

import io, os, re, sys, shlex, codecs

def warn(warning):
	sys.stderr.write("configure.coqoon.py: warning: %s\n" % warning)

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
		p._bits.extend(self._bits[i:])
		return p
	def drop_last(self, i):
		p = Path()
		p._bits.extend(self._bits[:i])
		return p

	def replace(self, i, s):
		p = Path()
		p._bits.extend(self._bits)
		p._bits[i] = s.rstrip("/")
		return p

	def __len__(self):
		return len(self._bits)

	def append(self, i):
		if len(i) != 0:
			p = Path()
			p._bits.extend(self._bits)
			p._bits.append(i.rstrip("/"))
			return p
		else:
			return self
	def append_path(self, i):
		if len(i._bits) != 0:
			p = Path()
			p._bits.extend(self._bits)
			p._bits.extend(i._bits)
			return p
		else:
			return self

	# Convenience file operations
	def open(self, mode = "r", encoding = "utf_8"):
		return io.open(str(self), mode = mode, encoding = encoding)
	def utime(times):
		os.utime(str(self), times)

	def __iter__(self):
		return self._bits.__iter__()

	def __str__(self):
		return "/".join(self)

def load_coq_project_configuration(path):
	configuration = []
	default_output = "bin"
	try:
		with io.open(path, mode = "r", encoding = "utf_8") as file:
			for line in file:
				if line.startswith("KOPITIAM_"):
					split_line = shlex.split(line)
					lp = shlex.split(split_line[2])
					configuration.append(lp)

					if lp[0] == "DefaultOutput":
						default_output = lp[1]
	except IOError:
		# If _CoqProject is missing, then use Coqoon's default settings
		configuration = [["SourceLoadPath", "src"],
		                 ["DefaultOutput", "bin"],
		                 ["AbstractLoadPath",
		                  "dk.itu.sdg.kopitiam/lp/coq/8.4"]]
	return (default_output, configuration)

# Read this project's configuration
default_output, configuration = load_coq_project_configuration("_CoqProject")

# This script can only support abstract load paths with some help from Coqoon,
# which produces a "configure.coqoon.vars" file specifying incomplete paths to
# the Coq load path entries that are associated with the abstract load paths
# required by this project

def load_vars(path):
	vs = []
	try:
		tokens = []
		with io.open(path, mode = "r", encoding = "utf_8") as file:
			tokens = shlex.split(file.read(), comments = True)
		while len(tokens) != 0:
			v = None
			if tokens[0] == "var":
				(v, tokens) = (tokens[0:3], tokens[3:])
			elif tokens[0] == "alp":
				if tokens[2] == "name":
					(v, tokens) = (tokens[0:4], tokens[4:])
				elif (tokens[2] == "include" or
				     tokens[2] == "include-recursive"):
					(v, tokens) = (tokens[0:5], tokens[5:])
				else:
					tokens = tokens[1:]
			else:
				# Skip this token in the hope that we'll
				# eventually get back in sync
				tokens = tokens[1:]
			if v != None:
				vs.append(v)
	except IOError:
		pass
	return vs

def structure_vars(vs):
	expected_vars = {} # Variable name -> human-readable description of
	                   # variable
	alp_names = {} # Abstract load path ID -> human-readable name
	alp_dirs_with_vars = [] # sequence of (abstract load path ID,
	                        # directory, coqdir, recursive)
	for i in vs:
		if i[0] == "var":
			expected_vars[i[1]] = i[2]
		elif i[0] == "alp":
			aid = i[1]
			if i[2] == "name":
				alp_names[aid] = i[3]
			elif i[2] == "include":
				alp_dirs_with_vars.append((aid, i[3], i[4],
						           False))
			elif i[2] == "include-recursive":
				alp_dirs_with_vars.append((aid, i[3], i[4],
						           True))
	return (expected_vars, alp_names, alp_dirs_with_vars)

vs = load_vars("configure.coqoon.vars")
if len(vs) == 0:
	warn("the \"configure.coqoon.vars\" file is missing, empty, or " +
	     "unreadable; non-trivial dependency resolution may fail")
expected_variables, alp_names, alp_directories_with_variables = \
	structure_vars(vs)

# XXX: what do we do about variables from other files incorporated by reference
# (i.e. ProjectLoadPath)?

variables = {} # Variable name -> user-specified value for variable

for i in sys.argv[1:]:
	match = re.match("^(\w+)=(.*)$", i, 0)
	if match:
		(var, value) = match.groups()
		if var in expected_variables:
			variables[var] = value
		else:
			warn("ignoring unexpected variable %s" % var)

for vn in expected_variables:
	if not vn in variables:
		affected_alps = []
		for aid, directory, _, _ in alp_directories_with_variables:
			name = "\"%s\"" % alp_names.get(aid, aid)
			if ("$(%s)" % vn) in directory and not name in affected_alps:
				affected_alps.append(name)
		aalps = None
		if len(affected_alps) == 1:
			aalps = affected_alps[0]
		elif len(affected_alps) > 1:
			aalps = ", ".join(affected_alps[0:-1]) + " and " + affected_alps[-1]
		warn(("the variable %s is not defined; " +
		      "dependencies on %s will not be resolved correctly") % (vn, aalps))

alp_directories = {} # Abstract load path ID -> sequence of (possibly resolved
                     # directory, coqdir, recursive)
for aid, directory, coqdir, recursive in alp_directories_with_variables:
	for vn, vv in variables.items():
		directory = directory.replace("$(%s)" % vn, vv)
	alp_elements = alp_directories.get(aid, [])
	alp_elements.append((directory, coqdir, recursive))
	alp_directories[aid] = alp_elements

# Find all source directories and their corresponding output directories
source_directories = [] # sequence of (source directory, output directory)
for i in configuration:
	if i[0] == "SourceLoadPath":
		entry = (i[1], i[2] if len(i) > 2 else default_output)
		source_directories.append(entry)

def extract_dependency_identifiers(f):
	identifiers = []
	for line in iter(f.readline, ""):
		for (_, ids) in re.findall("(?s)Require\\s+(Import\\s+|Export\\s+|)(.*?)\\s*\\.[$\\s]", line, 0):
			# Using shlex.split here is /technically/ cheating, but
			# it means we can handle both quoted identifiers and
			# multiple identifiers with the same code
			identifiers.extend(shlex.split(ids))
	return identifiers

# Populate the dependency map with the basics: .vo files depend on their source
deps = {} # Target path -> sequence of dependency paths
to_be_resolved = {}
for srcdir, bindir in source_directories:
	srcroot = Path(srcdir)
	binroot = Path(bindir)
	for current, dirs, files in os.walk(srcdir):
		srcpath = Path(current)
		binpath = binroot.append_path(srcpath.drop_first(len(srcroot)))
		if not os.path.isdir(str(binpath)):
			# Although the Makefile will be able to create this
			# folder, the load path expansion code needs it to
			# exist in order to work properly -- so, like Coqoon,
			# we create it in advance
			os.makedirs(str(binpath))
		for sf_ in filter(lambda f: f.endswith(".v"), files):
			sf = srcpath.append(sf_)
			bf = binpath.append(sf_ + "o")
			deps[str(bf)] = [str(sf)]

			# While we're here, stash away the identifiers of the
			# dependencies of this source file
			ids = None
			with sf.open() as file:
				ids = extract_dependency_identifiers(file)
			to_be_resolved[(str(sf), str(bf))] = ids

def expand_load_path(configuration):
	def expand_pair(coqdir, directory):
		expansion = []
		base = Path(directory)
		for current, _, _ in os.walk(directory):
			relative = Path(current).drop_first(len(base))
			sub = ".".join(relative)
			full = None
			if len(coqdir) == 0:
				full = sub
			elif len(sub) == 0:
				full = coqdir
			else:
				full = "%s.%s" % (coqdir, sub)
			expansion.append((full, current))
		return expansion
	load_path = []
	for i in configuration:
		if i[0] == "SourceLoadPath":
			s, b = (i[1], i[2] if len(i) > 2 else default_output)
			load_path.extend(expand_pair("", s))
			load_path.extend(expand_pair("", b))
		elif i[0] == "DefaultOutput":
			load_path.extend(expand_pair("", i[1]))
		elif i[0] == "ExternalLoadPath":
			directory = i[1]
			coqdir = i[2] if len(i) > 2 else ""
			load_path.extend(expand_pair(directory, coqdir))
		elif i[0] == "AbstractLoadPath":
			alp_elements = alp_directories.get(i[1])
			if alp_elements != None:
				for d, cd, r in alp_elements:
					if not os.path.isdir(d):
						# Unresolved directory; skip it
						continue
					elif not r:
						load_path.append((cd, d))
					else:
						load_path.extend(expand_pair(cd, d))
	return load_path

complete_load_path = expand_load_path(configuration) # sequence of (coqdir,
                                                     # resolved directory)

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
				deps[bf].append(str(p))
				break
		else:
			warn("%s: couldn't resolve dependency \"%s\"" % (str(sf), ident))

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
