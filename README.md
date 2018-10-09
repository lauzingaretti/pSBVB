
<h3 align="center";style="text-align: center;" markdown="1">pSBVB: Polyploid Sequence Based Virtual Breeding.</h3>

 <p align="center">
A flexible, efficient gene dropping algorithm to simulate sequence based population data and complex traits.
 <p align="center">
  Miguel Pérez-Enciso, Laura Zingaretti
 <p align="center">
 :email: miguel.perez@uab.es or laura.zingaretti@cragenomica.es
 <p align="center">
 with input from A Monfort 
:memo:

Purpose
=======

Polyploid sequence based virtual breeding (**pSBVB**) is a modification of SBVB software (Pérez-Enciso et al. 2016, <https://doi.org/10.1534/genetics.116.194878> ) that allows simulating traits of an arbitrary genetic complexity in polyploids. Its goal is to simulate complex traits and genotype data starting with a `vcf` file that contains the genotypes of founder individuals and following a given pedigree. The main output are the genotypes of all individuals in the pedigree and/or molecular relationship matrices (**GRM**) using all sequence or a series of SNP lists, together with phenotype data. The program implements very efficient algorithms where only the recombination breakpoints for each individual are stored, therefore allowing the simulation of thousands of individuals very quickly. Most of computing time is actually spent in reading the `vcf` file.


Main features
=============

-   Any number of traits.
-   Tool adapted to work with both, auto and allo-polyploid organisms.
-   Any number of QTNs, trait specific.
-   Any number of additive and dominant effects.
-   Can generate a correlation matrix to modelate meiosis in polyploid especies.
-   Can generate correlated allelic effects and frequencies.
-   Efficient algorithms to generate haplotypes and sample SNP genotypes.
-   Computes genomic relationship matrices for any number of SNP arrays simultaneously.
-   It allows to compute Genomic relationship matrix in several ways.
-   Any number of chromosomes, allows for sex chromosomes and varying local recombination rates, that can be sex specific.

Installation
============

The source code, manual and examples can be obtained from <https://github.com/lauzingaretti/pSBVB>

To compile:

``` r2
gfortran -O3 kind.f90 ALliball.f90 aux_sub11.f90 pSBVB.f90 -o sbvb -lblas
```

or

``` r2
make
```

To install in /usr/local/bin

``` r2
sudo make install
```

The program requires blas libraries but these are standard in any unix or OS mac system. We have tested pSBVB only in linux with gfortran compiler; intel ifort seems not working, but gfortran in mac OS looks ok.

Usage
=====
Full manual: https://lauzingaretti.github.io/pSBVB/
