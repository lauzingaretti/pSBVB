
<h3 align="center";style="text-align: center;" markdown="1">pSBVB: Polyploid Sequence Based Virtual Breeding.</h3>

 <p align="center">
A flexible, efficient gene dropping algorithm to simulate sequence based population data and complex traits.
 <p align="center">
  Miguel Pérez-Enciso, Laura Zingaretti
 <p align="center">
 :email: miguel.perez@uab.es or laura.zingaretti@cragenomica.es


:memo:

## Getting Started

Polyploid sequence based virtual breeding (**pSBVB**) is a modification of **SBVB** software (Pérez-Enciso et al. 2017) that allows simulating traits of an arbitrary genetic complexity in polyploids. Its goal is to simulate complex traits and genotype data starting with a ```vcf``` file that contains the genotypes of founder individuals and following a given pedigree. The main output are the genotypes of all individuals in the pedigree and/or molecular relationship matrices (GRM) using all sequence or a series of SNP lists, together with phenotype data. The program implements very efficient algorithms where only the recombination breakpoints for each individual are stored, therefore allowing the simulation of thousands of individuals very quickly. Most of computing time is actually spent in reading the ```vcf``` file. Future developments will optimize this step by reading and writing binary mapped files. The ```vcf``` file may not contain missing genotypes and is assumed to be phased. Manual: https://lauzingaretti.github.io/pSBVB/

### Main Features

* Any number of traits.
* Tool adapted to work with both, auto and allo-polyploid organisms.
* Any number of QTNs, trait specific.
* Any number of additive and dominant effects.
* Can generate a correlation matrix to modelate meiosis in polyploid especies.
* Can generate correlated allelic effects and frequencies.
* Efficient algorithms to generate haplotypes and sample SNP genotypes.
* Computes genomic relationship matrices for any number of SNP arrays simultaneously.
* It allow to compute Genomic relationship matrix in several ways.
* Any number of chromosomes, allows for sex chromosomes and varying local recombination rates, that can be sex specific.

### Prerequisites

The program requires **blas** libraries but these are standard in any unix or OS mac system. We have tested **pSBVB** only in linux with ```gfortran``` compiler; intel ifort seems not working, but ```gfortran``` in mac OS looks ok. To Ubuntu installing:

```
sudo apt-get install BLAS
```
Or a more minimalist set of packages can be installed with:
```
sudo apt-get install libblas-dev liblapack-dev
```

### Installing pSVBV

The source code, manual and examples can be obtained from:

<https://github.com/lauzingaretti/pSBVB>
To compile:
```gfortran -O3 kind.f90 ALliball.f90 aux_sub11.f90 pSBVB.f90 -o sbvb -lblas ```
or
```make ```

To install in  /usr/local/bin
```sudo make install```

## Running the tests

To run (assuming```.gen```  file is compressed):

```zcat file.gen  | psbvb -i sbvb.par```

Where ```sbvb.par``` is the parameter file (details follow). The intermediate steps are simply for **pSBVB** to read genotypes in suitable format, that is,


with alleles coded as _0/1_. To run the program with the same random seed:

```… | psbvb –i sbvb.par –seed iseed ```

where iseed is an integer number.

## Help summary

**pSBVB** requires a file parameter (i.e., ```.par ``` file). The sections of parameter file are controllated with command in UPPERCASE, e.g.,

**PEDFILE**
test.ped
**SNPFILE**
testSnp
**CHIPFILE**
chip.test

which specifies that the pedigree file is 'test.ped',SNPs are in testSnp and chip are in chip.test.

## Requirements

1.Mandatory Input files

**PEDFILE:** a pedigree file [toy example] (https://github.com/lauzingaretti/pSBVB/blob/master/src/toy.ped)

**QTNFILE:** a file with causal SNP positions [toy example] (https://github.com/lauzingaretti/pSBVB/blob/master/src/toy.qtn)

**SNPFILE:**  a file with a list of **SNP** positions in an array (up to 9 arrays can be analyzed simultaneously) [toy example] (https://github.com/lauzingaretti/pSBVB/blob/master/src/SNPfile)

**PARFILE:**  file with parameter options, see manual file for more details. To model ploidy, parfile include  **PLOIDY** option. If **PLOIDY** is higher than 2, you can include **MIMIC_DIPLOID** or **MIMIC_HAPLOID** option to calculate GRM matrix.

2.Optional

**MAPFILE:**  controls local recombination rates

## Output files
 
**OUTYFILE:**  phenotypes and genetic values per individual toy example

**OUTQFILE:**   QTN frequencies and ascribed variances toy example

**OUTGFILE:**   GRM files toy example

**OUTMFILE:**   Marker genotypes, optionally in Plink format toy example

### Controling genetic architecture
:octocat:
This is accomplished with **QTNFILE**. If **QTN** effects are not specified in **QTNFILE**, they can be simulated from uniform, normal or gamma distributions, specified in sections **QTNDISTA** and **QTNDISTD**. Narrow or broad sense heritabiilities are controlled with **H2** or **H2G** respectively. The software obtains actual genetic variances from individuals in base population and adjusts var e accordingly. See more in Manual File.

### Controlling output

By default, only phenotypes and breeding values are written, the rest are controlled with options **OUTQFILE**,  **OUTGFILE** and **OUTMFILE**. ```Plink``` files can be written with **OUTPLINK** option. **NOSEQUENCE** means sequence is not used for **GRM** or marker files.

### Restart

Sometimes one can be interested in running the same experiment but with different genetic architectures or different **SNP** arrays. **pSBVB** offers two convenient ways to do this as it may keep track of haplotypes so exactly the same genetic structure is preserved, **RESTART** and **RESTARTQTL**.

**NOTE 1** With **RESTART**, haplotypes, phenotypes and QTN effects are preserved. This is useful to implement selection.

**NOTE 2**  With **RESTARTQTN**, haplotypes are preserved but phenotypes and **QTN** effects are sampled again. **RESTARQTN** can be used to run different genetic architectures in the same haplotypes so results can be exactly comparable across models.

The program then writes a ```*hap``` file that contains all haplotype structure the first time is run.
### Toy example
This folder contains simplest example to check **pSBVB** format files from a tetraploid organism.

## Example

Contains an example consisting of 150 **SNPs** from the 47 lines from a toy  panel of  octoploid strawberry.


## Authors

**Miguel Pérez Enciso** - *Initial work*

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

**Citation**

L. Zingaretti, A. Monfort, M. Pérez-Enciso. 2018. pSBVB: a versatile simulation tool to evaluate genomic selection in polyploid species. Manuscript submitted for publication.

Pérez-Enciso, M., Forneris, N., de los Campos, G., & Legarra, A. (2016). Evaluating sequence-based genomic prediction with an efficient new simulator. Genetics, genetics-116.

Appendix: full list of options in parameter file
================================================

`NTRAIT` \#--&gt; specifies no. of traits (int, \[0\])

`PLOIDY` \#--&gt; specifies ploidy (int, \[2\])

`QTLFILE` !--&gt; file with qtl posns (chr& bp) add &dom effects can be defined in cols 3 & 4 (str)

`PEDFILE` !--&gt; !--&gt; file name with pedigree (only integer numbers allowed, 0 for unknown) pedfile

`SNPFILE` !--&gt; file with genotyped snps: chr, bp, can be repeated

`MAPFILE` !--&gt;recomb map file: chr, basepos, cm2Mb \[cm2Mb\_sex2\]

`HAPFILE` !--&gt; hap structure so program can be restarted with RESTART

`OUTPLINK` !--&gt; prints mkr in plink tpedformat

`OUTGFILE` !--&gt; GRM outfile

`OUTQFILE` !--&gt; output qtl file out\_q\_file

`OUTYFILE` !--&gt; y outfile outyfile

`OUTMFILE` !--&gt; output file with mkr data

outmfile

`GZIP` !--&gt; compress output files

`NBASE` !--&gt;nind which genotypes are read from `.vcf`or `.gen` file nbase

`H2G` !--&gt; broad heritability, repeated if multiple traits

`RHOQA` !--&gt; desired correlation between allele effect and frequency, repeated if multiple traits

`SIGNQTN` !--&gt; P of derived allele being deleterious (only with gamma) \[0.5\]

`QTLDISTA` !--&gt; QTL add effects are sampled from a distribution: u(niform), g(amma), n(ormal) \[u, l\_bound, u\_bound\] \[n, mu, var\] \[g, s, b\] ! repeated if multiple traits

`QTLDISTD` !--&gt; QTL dom effects are sampled from a distribution \[u, l\_bound, u\_bound\] \[n, mu, var\] \[g, s, b\] ! repeated if multiple traits

`CM2MB` !--&gt; cM to Mb rate, default cm2mb \[1.0\]

`MXOVER` !--&gt; Max no xovers, default 3

`RESTART` !--&gt; prepares files for new run of sbvb

`RESTARTQTL` !--&gt; restart qtl effects but keeps haplotype structure

`NOPRINTHAP` !--&gt; does not print hap file, eg, if no new haplotypes have been generated

`NOSEQUENCE` !--&gt; does not use sequence for GRM,

`EXPAND_BASEPOP` !--&gt; breeds new base individuals involving random mating for ntgen generations ! from nfam families

`ALLOPOLYPLOID` !--&gt; if PLOIDY is higher than 2 and you want to simulate an allopolyploid organism.

`MIMIC_DIPLOID` !--&gt; this option assumes than only presence or absence of the alternative allele can be ascertained for genotypes’ values higher than 2 (only should be on if organism is polyploid)

`MIMIC_HAPLOID` !--&gt; Assuming that only one allele can be distinguished for the others, i.e., that a given marker allele behaves as fully dominant.
