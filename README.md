
<h3 align="center";style="text-align: center;" markdown="1">pSBVB: Polyploid Sequence Based Virtual Breeding.</h3>

 <p align="center">
A flexible, efficient gene dropping algorithm to simulate sequence based population data and complex traits.
 <p align="center">
  Miguel Pérez-Enciso
 <p align="center">
 :email: miguel.perez@uab.es

<p align="center">
With collaborations from N. Forneris, G. de los Campos, A. Legarra and L Zingaretti

:memo:

## Getting Started

Polyploid sequence based virtual breeding (**pSBVB**) is a modification of **SBVB** software (Pérez-Enciso et al. 2017) that allows simulating traits of an arbitrary genetic complexity in polyploids. Its goal is to simulate complex traits and genotype data starting with a ```vcf``` file that contains the genotypes of founder individuals and following a given pedigree. The main output are the genotypes of all individuals in the pedigree and/or molecular relationship matrices (GRM) using all sequence or a series of SNP lists, together with phenotype data. The program implements very efficient algorithms where only the recombination breakpoints for each individual are stored, therefore allowing the simulation of thousands of individuals very quickly. Most of computing time is actually spent in reading the ```vcf``` file. Future developments will optimize this step by reading and writing binary mapped files. The ```vcf``` file may not contain missing genotypes and is assumed to be phased. Manual: https://github.com/lauzingaretti/pSBVB/blob/master/Manual.pdf; full html: https://lauzingaretti.github.io/pSBVB/

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

<https://github.com/mperezenciso/pSBVB>
<https://github.com/lauzingaretti/pSBVB>
To compile:
```gfortran -O3 kind.f90 ALliball.f90 aux_sub11.f90 psbvb.f90 -o sbvb -lblas ```
or
```make ```

To install in  /usr/local/bin
```sudo make install```

## Running the tests

To run (assuming```.vcf```  file is compressed):

```zcat file.vcf.gz | perl vcf2tped2.pl -hap | cut -d ' ' -f 1,4- | psbvb -isbvb.par```

Where ```sbvb.par``` is the parameter file (details follow). The intermediate steps are simply for **pSBVB** to read genotypes in suitable format, that is,

**allele1_snp1_ind1 allele2_snp1_ind1 allele3_snp1_ind1 ... alleleh_snp1_ind1 allele1_snp1_ind2 allele2_snp1_ind2 allele3_snp1_ind2 ... alleleh_snp1_ind2**

**allele1_snp2_ind1 allele2_snp2_ind1 allele3_snp2_ind1 ... alleleh_snp2_ind1 allele1_snp2_ind2 allele2_snp2_ind2 allele3_snp2_ind2 ... alleleh_snp2_indp**

with alleles coded as _0/1_. To run the program with the same random seed:

```… | psbvb –isbvb.par –seed iseed ```

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

For a complete help, check the manual file, and file [manual.sbvb]  ( https://github.com/lauzingaretti/pSBVB/blob/master/src/README.sbvb)...

## Requirements

1.Mandatory Input files

**PEDFILE:** a pedigree file [toy example] (https://github.com/lauzingaretti/pSBVB/blob/master/src/toy.ped)

**QTNFILE:** a file with causal SNP positions [toy example] (https://github.com/lauzingaretti/pSBVB/blob/master/src/toy.qtn)

**SNPFILE:**  a file with a list of **SNP** positions in an array (up to 9 arrays can be analyzed simultaneously) [toy example] (https://github.com/lauzingaretti/pSBVB/blob/master/src/SNPfile)

**PARFILE:**  file with parameter options, see manual file for more details. To model ploidy, parfile include  **PLOIDY** option. If **PLOIDY** is higher than 2, you can include **MIMICDIPLOID** option to calculate GRM matrix.

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

## Citation

M. Perez-Enciso, N. Forneris, G. de los Campos, A. Legarra. An efficient new simulator predicts minimal advantage of full sequence for genomic prediction. Submitted.


* Repo owner or admin
* Other community or team contact
