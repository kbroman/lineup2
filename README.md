## [R/lineup2](https://github.com/kbroman/lineup2)

[![R-CMD-check](https://github.com/kbroman/lineup2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kbroman/lineup2/actions/workflows/R-CMD-check.yaml)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/lineup2)](https://cran.r-project.org/package=lineup2)
[![zenodo DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4118180.svg)](https://doi.org/10.5281/zenodo.4118180)

[Karl W Broman](https://kbroman.org)

---

[R/lineup2](https://github.com/kbroman/lineup2) is an
[R](https://www.r-project.org) package with tools for detecting and
correcting sample mix-ups between two sets of measurements, such as
between gene expression data on two tissues. It's a revised
version of [lineup](https://github.com/kbroman/lineup), to be more
general and not so closely tied to the [R/qtl](https://rqtl.org)
package.


### Installation

You can install R/lineup2 from [CRAN](https://cran.r-project.org):

```r
install.packages("lineup2")
```


### Vignette

A vignette describing the use of the package is available
[on the web](https://kbroman.org/lineup2/lineup2.html).
Or view it from within R by loading the package and then using the
`vignette()` function.

```r
library(lineup2)
vignette("lineup2", package="lineup2")
```


### License

This package is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License, version 3, as
published by the Free Software Foundation.

This program is distributed in the hope that it will be useful, but
without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose.  See the GNU
General Public License for more details.

A copy of the GNU General Public License, version 3, is available at
<https://www.r-project.org/Licenses/GPL-3>
