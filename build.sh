#!/usr/bin/env bash
set -e

Rscript -e 'library("roxygen2"); roxygen2::roxygenise()'
R CMD build .
# --no-manual: skip PDF-manual generation, which needs a LaTeX toolchain (pdflatex/texi2dvi)
# not installed in this environment - unrelated to package correctness itself.
R CMD check --as-cran --no-manual SafeShiny_*.tar.gz
R CMD INSTALL SafeShiny_*.tar.gz
