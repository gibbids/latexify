# latexify
 
latexify is an R package that provides a function texarray() for converting R matrices to LaTeX arrays.

## Installation
You can install the development version of latexify from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("gibbids/latexify")
```

## Usage

### texarray

To use latexify, simply call the texarray function on your matrix:

``` r
library(latexify)

m <- matrix(1:9, nrow = 3)
texarray(m)
```

This will output the LaTeX code for the matrix:

<<<<<<< HEAD
``` 
\begin{array}{ccc}
 1 & 4 & 7 \\
 2 & 5 & 8 \\
 3 & 6 & 9 \\
\end{array}
```

<<<<<<< HEAD
You can customize the appearance of the table using the 'bracket', 'bar.pos', and inline arguments. See the package documentation for more information.
=======
You can customize the appearance of the table using the 'bracket.type', 'barpos', and 'inline arguments'. See the package documentation for more information.

### Contributing

Contributions to latexify are welcome! If you find a bug or have a suggestion for improvement, please submit an issue or pull request on GitHub.

### License

latexify is licensed under the GPL-3 license. See the LICENSE file for more information.