---
title: pSBVB Polyploid Sequence Based Virtual Breeding.
author: M P Enciso
markdown-pdf.scale: 0.5
fontsize: 11pt
geometry: margin=1in
export_on_save:
output:
  pandoc: true
  md_document:
    variant: markdown_github
  pdf_document:
    pandoc_args: [
      "--no-tex-ligatures"
    ]
  ebook: true
---

<h3 style="text-align: center;" markdown="1">pSBVB: Polyploid Sequence Based Virtual Breeding.</h3>

 <p align="center">
A flexible, efficient gene dropping algorithm to simulate sequence based population data and complex traits.
 <p align="center">
  Miguel Pérez-Enciso
 <p align="center">
 <miguel.perez@uab.es>

<p align="center">
With collaborations from N. Forneris, G. de los Campos, A. Legarra and L Zingaretti

### Purpose
Polyploid sequence based virtual breeding (**pSBVB**) is a modification of **SBVB** software (Pérez-Enciso et al. 2017) that allows simulating traits of an arbitrary genetic complexity in polyploids. Its goal is to simulate complex traits and genotype data starting with a ```vcf``` file that contains the genotypes of founder individuals and following a given pedigree. The main output are the genotypes of all individuals in the pedigree and/or molecular relationship matrices (GRM) using all sequence or a series of SNP lists, together with phenotype data. The program implements very efficient algorithms where only the recombination breakpoints for each individual are stored, therefore allowing the simulation of thousands of individuals very quickly. Most of computing time is actually spent in reading the ```vcf``` file. Future developments will optimize this step by reading and writing binary mapped files. The ```vcf``` file may not contain missing genotypes and is assumed to be phased.

### Main features

*Any number of traits.
*Tool adapted to work with both, auto and allo-polyploid organisms.
*Any number of QTNs, trait specific.
*Any number of additive and dominant effects.
*Can generate a correlation matrix to modelate meiosis in polyploid especies.
*Can generate correlated allelic effects and frequencies.
*Efficient algorithms to generate haplotypes and sample SNP genotypes.
*Computes genomic relationship matrices for any number of SNP arrays simultaneously.
*It allow to compute Genomic relationship matrix in several ways.
*Any number of chromosomes, allows for sex chromosomes and varying local recombination rates, that can be sex specific.

### Installation

The source code, manual and examples can be obtained from
<https://github.com/mperezenciso/sbvb0>

To compile:

```gfortran -O3 kind.f90 ALliball.f90 aux_sub11.f90 psbvb.f90 -o sbvb -lblas ```

or

```make ```

To install in  /usr/local/bin

```sudo make install```

The program requires **blas** libraries but these are standard in any unix or OS mac system. We have tested **pSBVB** only in linux with ```gfortran``` compiler; intel ifort seems not working, but ```gfortran``` in mac OS looks ok.

### Usage

Having trouble with Pages? Check out our [documentation](https://help.github.com/categories/github-pages-basics/) or [contact support](https://github.com/contact) and we’ll help you sort it out.
