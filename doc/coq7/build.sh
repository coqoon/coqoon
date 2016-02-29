#!/bin/sh

pdflatex coq7.tex && \
	bibtex coq7.aux && \
for i in 1 2 3
do
	pdflatex coq7.tex
done
