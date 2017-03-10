# Pearson and Lee (1903) data

Data extracted from Karl Pearson and Alice Lee (1903)
[On the laws of inheritance in man: I. Inheritance of physical characters](http://www.jstor.org/stable/2331507).
Biometrika 2: 357-462

The `.txt` files are transcribed versions of various tables from the
paper.

The `.R` file contains a function, `interp_pearson()` for converting
the data to interpolated (and slightly randomized) values for pairs of
individuals. It reads a `.txt` file and writes a `.csv` file.

The `.csv` files are files derived from `interp_pearson()`.
