#!/bin/sh

pdflatex coq6.tex && \
	bibtex coq6.aux && \
for i in 1 2 3
do
	pdflatex coq6.tex
done
