Purpose
=======

Polyploid sequence based virtual breeding (**pSBVB**) is a modification of SBVB software (Pérez-Enciso et al. 2016, <https://doi.org/10.1534/genetics.116.194878> ) that allows simulating traits of an arbitrary genetic complexity in polyploids. Its goal is to simulate complex traits and genotype data starting with a `vcf` file that contains the genotypes of founder individuals and following a given pedigree. The main output are the genotypes of all individuals in the pedigree and/or molecular relationship matrices (**GRM**) using all sequence or a series of SNP lists, together with phenotype data. The program implements very efficient algorithms where only the recombination breakpoints for each individual are stored, therefore allowing the simulation of thousands of individuals very quickly. Most of computing time is actually spent in reading the `vcf` file. Future developments will optimize this step by reading and writing binary mapped files. The `vcf` file may not contain missing genotypes and is assumed to be phased.

![General Software' View](generalview.png)

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

The source code, manual and examples can be obtained from <https://github.com/lzingaretti/pSBVB>

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

<span style="color:#cc66ff; font-family:Georgia; font-weight: bold ">USAGE:</span>

To run: if you have a `.vcf` file

``` r2
 file.vcf | pSBVB -isbvb.par
```

if you have a .gen file

``` r2
 file.gen | pSBVB -isbvb.par
```

with alleles coded as *0/1*. In the gen file, the first two rows contain the chromosome and SNP position (in base pairs), and the following columns contain the genotypes (coded *0,1,…h*) for each of the individuals of the founder population. To run the program with the same random seed:

``` r2
… | pSBVB -isbvb.par –seed iseed
```

where iseed is an integer number

Parameter file
==============

The parameter file controls all **pSBVB** behavior. It consists of a list of sections in UPPER CASE (in any order) followed in the following line by the required data e.g.,

**QTNFILE**

sbvb.qtl

tells the program that **QTN** specifications are in sbvb.qtl file. Comments can be mixed starting with \# or ! A full list of options in the parameter file is in [Appendix](#/Apendix)).The main ones are:

The number of traits to be generated is especified (by default is one):

**NTRAIT**

ntraits

in parameter file. Otherwise this section is not needed. **pSBVB** requires the user to provide the list of causal SNPs (**QTNs**) as specified in **QTNFILE** section. The format of the QTN file is an orderedfile containing:

``` r2
chrom pos
```

<span style="color:#ffffff; font-family:Georgia; font-weight: bold; background-color: #cc66ff">WARNING:</span> chromosome ids must be integer consecutive numbers

or

Purpose
=======

Polyploid sequence based virtual breeding (**pSBVB**) is a modification of SBVB software (Pérez-Enciso et al. 2016, <https://doi.org/10.1534/genetics.116.194878> ) that allows simulating traits of an arbitrary genetic complexity in polyploids. Its goal is to simulate complex traits and genotype data starting with a `vcf` file that contains the genotypes of founder individuals and following a given pedigree. The main output are the genotypes of all individuals in the pedigree and/or molecular relationship matrices (**GRM**) using all sequence or a series of SNP lists, together with phenotype data. The program implements very efficient algorithms where only the recombination breakpoints for each individual are stored, therefore allowing the simulation of thousands of individuals very quickly. Most of computing time is actually spent in reading the `vcf` file. Future developments will optimize this step by reading and writing binary mapped files. The `vcf` file may not contain missing genotypes and is assumed to be phased.

![General Software' View](generalview.png)

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

The source code, manual and examples can be obtained from <https://github.com/lzingaretti/pSBVB>

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

USAGE
=====

As input, the software requires Chrom, Position and genotypes as follow:

``` r2
Chrom Pos I1_c1 I1_c2 ... I1_cp ... In_c1 In_c2 ... In_cn
1     453   1     0   ...   0   ...   1     1   ...   1
1     530   1     1   ...   0   ...   0     1   ...   1
.......................................................
.......................................................
12   5000   0     1   ...   1   ...   1     0   ...   1   
```

Note that Chrom and positions should be sorted.

To run: if you have a `.vcf` file (file could be compress vcf.gz) or in genotypes format, i.e. the first two rows contain the chromosome and SNP position (in base pairs), and the following columns contain the genotypes (coded *0,1,…h*), you should run the next line:

``` r2
cat file.vcf | python3 vcftogen.py -p Ploidy | ~/Dropbox/DoctoradoCRAG/paper-1/article/software-help/src/sbvb  -i file.par
```

if you already have the standar input file (.gen)

``` r2
 file.gen | pSBVB -isbvb.par
```

To run the program with the same random seed:

``` r2
… | pSBVB -isbvb.par –seed iseed
```

where iseed is an integer number

Phenotype simulation
====================

**pSBVB** takes ploidy into account to generate the phenotypes and incorporates several options to generate the molecular relationship matrix that are pertinent to polyploids. In a diploid organism, the phenotype for *i*<sub>*t**h*</sub> individual can be simulated from:

$y\_{i}=\\mu + \\sum\_{j=1}^{Q} \\gamma\_{ij}\\alpha\_{j} + \\sum\_{j=1}^{Q} \\delta\_{ij}d\_{j} + \\epsilon\_{i}$

Where *μ* is the mean general, *α* is the additive effect of **<sub>*t**h*</sub> locus, that is, half the expected difference between homozygous genotypes, *γ*<sub>*i**j*</sub> takes values -1, 0 and 1 for homozygous, heterozygous and alternative homozygous genotypes, respectively. *d*<sub>*j*</sub> is the dominance effect of *j*<sub>*t**h*</sub> locus, and *δ*<sub>*i**j*</sub> takes value 1 if the genotype is heterozygous, 0 otherwise, and *ϵ*<sub>*i*</sub> is a normal residual. For polyploids, the phenotype of individual *i* (*y*<sub>*i*</sub>) (equivalent equation) is simulated from:

$y\_{i}=\\mu + \\sum\_{j=1}^{Q} \\eta\_{ij}\\alpha\_{j} + \\sum\_{j=1}^{Q} \\phi\_{ij}d\_{j} + \\epsilon\_{i}$

where *η*<sub>*i**j*</sub> is the number of copies of the alternative allele (coded say as 1) minus half the ploidy (*h*/2) for *j*-th locus and *i*-th individual, and *α*<sub>*j*</sub> is therefore the expected change in phenotype per copy of allele ‘1’ in the *j*-th locus. In polyploids, as many dominance coefficients as ploidy level (*h*) minus two can technically be defined. However, this results in an over-parameterized model that is of no practical use. Here instead we define the *ϕ*<sub>*i**j*</sub> parameter as the minimum number of copies of allele 1 such that the expected phenotype is *d*<sub>*j*</sub>. By default, **pSBVB** uses *ϕ*<sub>*i**j*</sub> = 1 , that is, any genotype having at least one allele ‘1’ and ‘0’ has the expected phenotypic value *d*<sub>*j*</sub>. (see Figure 1 on Zingaretti, et al., 2018)

Parameter file
==============

The parameter file controls all **pSBVB** behavior. It consists of a list of sections in UPPER CASE (in any order) followed in the following line by the required data e.g.,

**QTNFILE**

sbvb.qtl

tells the program that **QTN** specifications are in sbvb.qtl file. Comments can be mixed starting with \# or ! A full list of options in the parameter file is in [Appendix](#/Apendix)).The main ones are:

The number of traits to be generated is especified (by default is one):

**NTRAIT**

ntraits

in parameter file. Otherwise this section is not needed. **pSBVB** requires the user to provide the list of causal SNPs (**QTNs**) as specified in **QTNFILE** section. The format of the QTN file is an orderedfile containing:

``` r2
chrom pos
```

<span style="color:#ffffff; font-family:Georgia; font-weight: bold; background-color: #cc66ff">WARNING:</span> chromosome ids must be integer consecutive numbers

or

``` r2
chrom pos add_eff_Trait_1  add_eff_Trait_2 ... add_eff_Trait_n
```

or to especified additive and dominant effects:

``` r2
chrom pos add_eff_Trait_1  add_eff_Trait_2 ...add_eff_Trait_n dom_eff_Trait_1  dom_ef_Trait_2  ...dom_eff_Trait_n
```

where *chr* is chromosome and *pos* is position in base pair, *a**d**d*<sub>*e**f**f*</sub> is additive effect, i.e the effect of homozygous alleles and *d**o**m*<sub>*e**f**f*</sub> is the heterozygous effect.

<span style="color:#3358ff; font-family:Georgia; font-weight: bold;  background-color: #ffcccc">NOTE:</span> Irrespective of the number of traits, the complete list of QTNs must be specified- If a QTN does not affect a phenotype, add and dom effects should be set to 0 for that trait.

<span style="color:#ffffff; font-family:Georgia; font-weight: bold;  background-color: #cc66ff" "="">WARNING:</span> **QTN** position must coincide with one **SNP** position in the `vcf` file, otherwise it is not considered.

If QTN effects are not provided, they can be simulated specifying using an uniform, normal or gamma distribution. QTNDISTA is to simulates adittive effects, whereas QTNDISTD controls dominant ones.

**QTNDISTA**

\[u a b\] \[n mu var\] \[g s b\]

and

**QTNDISTD**

\[u a b\] \[n mu var\] \[g s b\]

in parameter file.

where 'u a b' means effects are sampled from a uniform distribution *U* ∼ (*a*, *b*), 'n mu var' from a normal distribution *N* ∼ (*m**u*, *v**a**r*) and 'g s b' from a gamma *γ* ∼ (*s*, *b*). For a gamma distribution, you can specify the probability p that a derived allele decreases the phenotype with:

**PSIGNQTN**

p

The default value is 50%. By default, effects are sampled independently of frequency, i.e., half effects are + and the rest are -, but it is possible to generate a correlation (rho) using the following parameter:

**RHOQA**

rho

where rho is the desired correlation between QTN additive effects and their frequencies. This option can be useful to simulate past selection (Pérez-Enciso et al., 2016), since selection induces a negative correlation between frequency and effect. By default, rho is 0.

For instance, if you want to simulate the additive effects using a Uniform distribution (*U* ∼ (0.2, 0.6)), your parameter file should have a section as:

``` r2
QTNDISTA

u 0.2 0.6
```

The broad sense heritability is specified as:

**H2G**

h2

The genotypes from the base population (in the `vcf` file) are used to adjust the environmental variance such that the heritability is as desired.

For multiple traits, the fields **H2** or **H2G**, **RHOQA**, and **QTLDISTA** and **QTLDISTD** must be repeated, eg, for two traits:

**H2G**

0.5

0.23

**RHOQA**

0

-0.4

**QTNDISTA**

u -0.2 0.2

g 1 0.5

which means that the firts trait should have a heritability of 0.5, no correlation between genetic effects and frequencies and that addtive effects are sampled from an uniform distribution (0.2, 0.2), whereas the second trait have a heritability of 0.23, a negative correlation −0.4 between genetic effects and frequencies with genetic effects following a gamma distribution with parameters (1, 0.5).

Recombination in polyploids
===========================

The ploidy level must be specified with section

PLOIDY

h

in the parameter file. By default, pSBVB assumes autopolyploidy and permits recombination between each homeolog chromosome pair with equal probability. Strict alloploidy is specified with

ALLOPLOIDY

Intermediate rates of recombination between homeolog pairs can be specified with

RHOPLOIDY

rho\_elements

where rho\_elements is a vector of hxh elements specifying the probability of recombination between i and j homeologs. The diagonal (P of recombining with itself) is set to 0 by the program. For instance, if the organism is tetraploid, you can set rho as:

$$
\\begin{bmatrix}
 0 & 0.5 & 0.2 & 0.3 \\\\
0.5 & 0 & 0.3 & 0.2 \\\\
0.2 & 0.3 & 0 & 0.5 \\\\
0.3 & 0.2 & 0.5 & 0 \\\\
\\end{bmatrix}
$$

Pedigree file (PEDFILE)
=======================

The format is id id\_father id\_mother

where all ids must be consecutive integers, 0 if father or mother unknown. The number of individuals in the `vcf` file must be specified with section:

**NBASE**

nbase

in the parfile. The pedigree file must contain the first rows as

``` r2
  1   0  0 
  2   0  0 
 ...  0  0 
nbase 0  0  
```

that is, those in `vcf` file are assumed to be unrelated.

Recombination map files (MAPFILE)
=================================

By default, **pSBVB** assumes a cM to Mb ratio of 1. This ratio can be changed genomewide with **CM2MB** section in the par file. In addition, local recombination rates can be specified with the **MAPFILE** section. The mapfile takes format

**MAPFILE**

``` r2
chr  last_bp  local_cm2mb 
```

where **local\_cm2mb** is the recombination rate between **last\_bp** and previous bound (1 bp if first segment) , or

``` r2
chr last_bp local_cm2mbMales local_cm2mb_females
```

SNP array files (SNPFILE)
=========================

**pSBVB** can compute the genomic relationship matrix (GRM) using all sequence data and/or specific SNP subsets to mimic different genotyping arrays. The lists of SNPs are specified in the SNPFILE sections. Several **SNP** lists can be analyzed in the same run repeating the **SNPFILE** section in the par file. Each **SNP** file has the same format as the QTN file, i.e., chromosome and base pair position, as idicated:

``` r2
chrom pos 
```

Output
======

Output is controlled in the parameter file with sections OUTGFILE, OUTYFILE, OUTQFILE, OUTGFILE and OUTMFILE:

### GRM files (GRMFILE)

pSBVB computes one GRM for each of the SNP files plus the sequence data. By default, equation 1 in Zingaretti, et al. (2018). if you add the command **MIMIC\_DIPLOID**, Genomic matrix is computed mimic diploid. Only 0, 1 and 2 or more copies of a given allele can be distinguished. In this case, all genotypes with values larger than 2 are assumed to be observed as '2',then molecular matrix has elements ranging between 0 and 2 and ploidy is set to 2.

if you add **MIMIC\_HAPLOID** to parameter file, it is assumed that only one full homozygous can be distinguished for the rest of genotypes, then the molecular matrix has elements ranging between 0 and 1 and ploidy is set to 1. See Zingaretti et al for details.

### OUTYFILE

Specifies the name of the phenotype / genotype file, which has format:

``` r2
 id y  [(add__eff)_i , i=1,..,ntraits] [(add_eff+ dom_eff)_i, i=1,..,ntraits]
```

where *a**d**d*<sub>*e**f**f*</sub> is the first sum in equation of **pSBVB** software, shows above and *d**o**m*<sub>*e**f**f*</sub> is the second term. For several traits, first are printed all add effects for every trait, following add+dom.

### OUTQFILE format (contains **QTN** info):

``` r2
 chr pos freq_{base} [(add_eff_i) i=1,..,ntraits] [(dom_eff_i) i=1,..,ntraits]
```

where *c**h**r* is chromosome, *p**o**s* is *Q**T**N* bp position, *f**r**e**q*<sub>*b**a**s**e*</sub> is frequency in `.vcf` file, freq is frequency along the pedigree, plus additive, dominant effects and add variance (2*p**q**α*<sup>2</sup>) contribution for each locus by trait.

### OUTGFILE format (contains GRM, one per SNPFILE plus sequence)

A matrix of *n* × *n*, where *n* is the number of individuals in the pedigree. As many outgfiles as snpfiles are written with subscripts .1, .2 etc. .0 corresponds to sequence. To avoid using sequence, add **NOSEQUENCE** command in parfile.

### OUTMFILE

format (contains genotypes for evey SNP file and sequence, in plink format optionally using **OUTPLINK** in parfile). As many outmfiles as snpfiles are written with subscripts .1, .2 etc. .0 corresponds to sequence. To avoid using sequence, **NOSEQUENCE** in parfile

Outqfile, outqtn, GRM and marker files are written only if the respective sections **OUTQFILE**, **OUTGFILE** and **OUTMFILE** appear in the `.par` file. Note in particular that **OUTMFILE** with sequence can be huge! To avoid printing sequence info, use

**NOSEQUENCE**

in par file.

<span style="color:#3358ff;; font-family:Georgia; font-weight: bold; background-color: #ffcccc">NOTE:</span> To compress marker output, include **GZIP** option in parfile.

Restart the program keeping the same haplotypes
===============================================

Sometimes one can be interested in running the same experiment but with different genetic architectures or different **SNP** arrays. The program offers two convenient ways to do this as it may keep track of haplotypes so exactly the same genetic structure is preserved, **RESTART** and **RESTARTQTL** options in `.par` file.

1.With **RESTART**, haplotypes, phenotypes and **QTN** effects are preserved. This is useful to implement selection.

2.With **RESTARTQTN**, haplotypes are preserved but phenotypes and **QTN** effects are sampled again. **RESTARQTN** can be used to run different genetic architectures in the same haplotypes so results can be exactly comparable across models.

The program then writes a `.hap` file that contains all haplotype structure the first time is run. When **pSBVB** is called again with say another **SNPFILE**, then individuals have the same haplotypes as in previous runs and a new **GRM** can be generated with the new **SNP** file. An important application is to run selection. In fact, **pSBVB** can be run with different pedigree files and the **RESTART** option. **pSBVB** generates only new haplotypes for those individuals not in current `.hap` file. In a selection scheme, the user should add a new generation pedigree to current pedfile with the offspring of selected individuals. In the new run, **pSBVB** generates haplotypes and phenotypes for the new offspring.

<span style="color:#3358ff; font-family:Georgia; font-weight: bold">IMPORTANT:</span> The `.hap` file is used only if **RESTART** is included in parfile. If no `.hap` file is present, a new one is generated the first time. You can check that **RESTART** is in use checking, e.g, that all phenotypes are the same in different runs.

<span style="color:#ffffff; font-family:Georgia; font-weight: bold;  background-color: #cc66ff">WARNING:</span> **RESTARTQTN** is logically not suitable for selection, since effects are sampled anew in each run.

### Expanding the base population

Very often, complete sequence is available only for very few individuals. **pSBVB** implements an automatic option to generate additional individuals by randomly crossing the available ones and random breeding for a pre specified number of generations. To use this feature, the pedigree file must contain larger number of individuals with unknown parents than in the `vcf` file. For instance, assume your `vcf` file contains only four individuals and the pedfile is

``` r2
  1   0  0 
  2   0  0 
 ...  0  0 
 20   0  0 
 21   1  7 
 ...  .  . 
```

Then individuals 5-20 are generated by randomly crossing 1-4 ids, from id 21 onwards, normal pedigree gene dropping is implemented. The option in parfile is

**EXPAND\_BASEPOP**

``` r2
ntgen nfam 
```

which means that the new individuals are generated by crossing nfam individuals of the `vcf` file for ntgen generations.

Toy Examples
============

### Prepare your files

To facilitate the software' usability, we have written some additional functions using `R` and `python` to generate the requested files (pedigree, qtn, chip) and to transform `vcf` file into `.gen`. We also incorporate two functions to generate the numerator relationship matrix and to performed predictive ability (PA) from GBLUP model.

### Function to create a pedigree to use as .ped file

Pedigree file is required to simulate. The number of founder has to be equal or higher than the number of individuals in .gen (or `.vcf`) file. The founders are those individuals with genomic information.

We provide a R function to generate pedigree file. You can choose any number of founders, generations and individuals by generation.

The following pedigree file has 47 founders, 8 generations, 100 individuals of each generation from 1 to 7.

``` r2
source("/Additional_functions/pedigree.R")
M<-pedgenerator(100,4,c(rep(100,3),150),
   path="/path_to_file/",exclude=47)
```

### To generate a pedigree based relationship matrix

We implemented a R function to generate a Relationship matrix to compare with genomic relationship matrix. Our function could be used to compute both, additive and dominant relationship matrices. As input, a pedigree file is needed.

``` relationship
source("/Additional_functions/RelationshipMatrix.R")
data<-read.table("Path_to_File_st.ped",header=FALSE)
A<-RelMatrix(data,dominance=FALSE,path= "path_to_file"")
```

Run pSBVB
=========

Once you have a genotypes, a pedigree, a list with SNPs and a Chip file. You can to create several `.par` files to perform simulations with different options.

Here, we give two toy dataset with examples, which are in `/toy_potato` and `/toy_strawberry` folders.

You could run several examples simultaneosly, by executing the `.sh` files in `/toy_potato` and `/toy_strawberry` folders as indicates here:

``` bash
cat toy_st.gen | sbvb -i file_1.par
```

or using a .sh file:

``` r2
cd /path_to_/toy_strawberry (or path_to/toy_potato)
./run_toy_st.sh 
```

The following images show how it works:
![Run the program from command line](example.run.png)

![Run the program several times simultaneosly using a bash script](runbash.png)

GBLUP prediction
================

There are numerous GS methods that use genome-wide markers to predict breeding values and address the large p small n problem. Here, breeding values were predicted using GBLUP, which is a genomic-based extension of traditional BLUP (Henderson, 1984) and equivalent to Ridge Regression Model (RR-BLUP). The model is:

The R script to perform Predictive abilities is /Additional\_Functions/GBlupFunction.R

Toy Potato dataset
==================

We used a subset of 396 SNPs and 150 individuals from Enciso-Rodriguez et al. (2018) with genotypes coded between 0 and 4 (the potato ploidy level). We used these genotypes to generate a `vcf`- like file with random phases. SNP positions were obtained from Rosyara et al.

``` r
source("/Additional_Functions/pedigree.R")
source("/Additional_Functions/RelationshipMatrix.R")
source("/Additional_Functions/GBlupFunction.R")
source("/Additional_Functions/generate_vcf_from_gen.R")
```

We created a function to transform genotypes into `vcf` format. To do that, a dataset with genotypes (data varying between 0 and ploidy level) and a map file containing the SNP’s physical coordinates are needed. We used genotypes from potato database (<https://figshare.com/articles/Supplemental_Material_for_Enciso-Rodriguez_et_al_2018/6262214>). Note that our function randomly generates the phases in the `vcf` file. In order to generate linkage disequilibrium, we used pSBVB parameters EXPAND\_BASEPOP and INDFIRST, which simultaneously exclude the initial individuals and generate a new set of founders.

Furthermore, the `toy_potato/FilesGenerator.R` file have all the functions needed to create a Chip, pedigree, QTNs location files and run potato example.

``` r
# go to file containers folder
# setwd("/toy_potato")
map<-read.table("Sol_tet_mapa.map",sep="\t")
#read genotype
G<-read.delim("Pot_gen.gen",sep="\t")
#select a sub-sample from whole dataset 
G=G[sample(c(1:nrow(G)),150),]
G=G[,sample(c(1:ncol(G)),500)]
G<-G[,colnames(G)%in%map[,1]]
dim(G)
```

    ## [1] 150 412

``` r
#generate genotypes vcf format 
A=GenotoVcf(G,p=4,map,path="NULL")
```

    ## Loading required package: data.table

``` r
library(ggplot2)
# data<-read.table("File_st.ped",header=FALSE)

#check the dimensions of dataset 
#generate relationship matrix
dim(data)
```

    ## [1] 700   3

``` r
A<-RelMatrix(data,dominance=FALSE)

## make sure you're in the folder toy_potato 

#G<- read.table("/results_potato/Y.grm.1")
#y<- read.table("/results_potato/Y.outy")
RelMatrix<-A[-c(1:150),-c(1:150)]


h2=0.5
ntraits=1
# split testing and training(the last 150 individuals are used to test)
make_predictions=c(401:550)
S<-GBlup_predict(G=G,y=y,h2=h2,make_predictions=make_predictions,ntraits=ntraits)
S$rho
```

    ## [1] 0.5668883

``` r
Phenohat<-S$uhat[make_predictions] + S$mean
Pheno<-S$yout_corr[make_predictions]

datos<-data.frame(Phenohat,Pheno)
colnames(datos)<-c("Yhat","Y")

#plotting predictions
ggplot(datos, aes(y=Yhat, x=Y,colour="red")) +
  ggtitle("Estimated values for testing set - Genomic matrix")+
  geom_point(size=0.6) + scale_shape_manual(values=c(2,4)) + 
  geom_abline(intercept=lm(Yhat ~ Y,data=datos)$coefficients[1], slope=lm(Yhat ~ Y,data=datos)$coefficients[2])+
  #scale_x_continuous(limits = c(min(datos[,1]), max(datos[,1])))+
  #scale_y_continuous(limits = c(min(datos[,2]), max(datos[,2])))+
  
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position ="none")
```

![](help_addfunctions_version2_files/figure-markdown_github/unnamed-chunk-28-1.png)

``` r
#Assesing PA using Relationship Matrix
SR<-GBlup_predict(G=RelMatrix,y=y,h2=h2,make_predictions=c(401:550),ntraits=ntraits)
SR$rhoM
```

    ## [1] 0.3927855

``` r
#plotting predictions with relationship matrix
Pheno<-SR$yout_corr[make_predictions]
datos_r<-data.frame(Phenohat,Pheno)
colnames(datos_r)<-c("Yhat","Y")

ggplot(datos_r, aes(y=Yhat, x=Y,colour="red")) +
  geom_point(size=0.6) + scale_shape_manual(values=c(2,4)) + 
  geom_abline(intercept=lm(Yhat ~ Y,data=datos_r)$coefficients[1], slope=lm(Yhat ~ Y,data=datos_r)$coefficients[2])+
   ggtitle("Estimated values for testing set - N Relationsip matrix")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position ="none")
```

![](help_addfunctions_version2_files/figure-markdown_github/unnamed-chunk-28-2.png)

Toy Strawberry dataset
======================

### Running toy\_strawberry using three heritabilities parameters

The first example (in path `/toy_strawberry/file_1.par`) incorporate three "H2G" parameters of 0.3, 0.4 and 0.5 and 150 causals SNPs to simulate the phenotype. The three phenotypes with additive effects are simulated simultaneosly. The Genomic Relationship matrix is generated using G- total, then the M values varying between 0 and 8. We show the PA obtained by the thirth output:

``` r
library(ggplot2)
## make sure you're in the folder toy_strawberry and you has been executed /run_toy_st.sh file, which generates all examples folder and outputs 
#G_1<- read.table("/results_example1/Y.grm.1")
#y_1<- read.table("/results_example1/Y.outy")

h2=c(0.3,0.4,0.5)
ntraits=3
#select training population 
make_predictions=c(353:503)

S<-GBlup_predict(G=G_1,y=y_1,h2=h2,make_predictions=make_predictions,ntraits=ntraits)

#Predictive ability computed with GModel 
S[[3]]$rho
```

    ## [1] 0.4989232

``` r
Phenohat<-S[[3]]$uhat[make_predictions] + S[[3]]$mean
Pheno<-S[[3]]$yout_corr[make_predictions]

datos<-data.frame(Phenohat,Pheno)
colnames(datos)<-c("Yhat","Y")

ggplot(datos, aes(y=Yhat, x=Y,colour="red")) +
  geom_point(size=0.6) + scale_shape_manual(values=c(2,4)) + 
  ggtitle("Estimated values for testing set - Genomic matrix") +
  geom_abline(intercept=lm(Yhat ~ Y,data=datos)$coefficients[1], slope=lm(Yhat ~ Y,data=datos)$coefficients[2])+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position ="none")
```

![](help_addfunctions_version2_files/figure-markdown_github/unnamed-chunk-30-1.png)

### Compute predictive ability from model with dominant effects

This model includes dominant effects from an uniform distribution with parameters a=0.2, b=0.6

``` r
# read G and y from /toy_strawberry/results_example4/
#G_2<- read.table("Y.grm.1")
#y_2<- read.table("Y.outy")
# read relationship matrix (R) from /toy_strawberry folder 
#R<-read.table("Relationship.mat")

# to eliminate the base population (first 47 individuals) 
R<-R[c(48:550),c(48:550)]

h2=0.5
ntraits=1
# split test and train 
make_predictions=c(354:503)

#genetic and predigree based model
S<-GBlup_predict(G=G_2,y=y_2,h2=h2,make_predictions=c(354:503),ntraits=ntraits)
SR<-GBlup_predict(G=R,y=y_2,h2=h2,make_predictions=c(354:503),ntraits=ntraits)
## print the PA from GENETIC MODEL 
S$rhoM
```

    ## [1] 0.4605796

``` r
# print the PA from RELATIONSHIP MODEL
SR$rhoM
```

    ## [1] 0.4197223

### Computing PA from MIMIC\_DIPLOID G matrix

This example includes a heritability parameter of 0.5 and 150 QTN’s. The Genetic matrix was computed with MIMIC\_DIPLOID option.

``` r
# read G and y from /toy_strawberry/results_example2/
#G_3<- read.table("Y.grm.1")
#y_3<- read.table("Y.outy")
# Numerator relationship matrix is the same than before 

h2=0.5
ntraits=1
make_predictions=c(354:503)

S<-GBlup_predict(G=G_3,y=y_3,h2=h2,make_predictions=c(354:503),ntraits=1)
SR<-GBlup_predict(G=R,y=y_3,h2=h2,make_predictions=c(354:503),ntraits=1)
#correlation with Genetic Matrix
S$rhoM
```

    ## [1] 0.434933

``` r
#correlation with Numerator relationship matrix
SR$rhoM
```

    ## [1] 0.4971778

### Computing PA from MIMIC\_HAPLOID G matrix

This snippet includes h2= 0.5 and 150 QTN’s The Genetic matrix was computed with MIMIC\_HAPLOID option.

``` r
# example from G generated through mimic haploid option  (results on /toy_strawberry/results_example8)
#G<- read.table("Y.grm.1")
#y<- read.table("Y.outy")
#Numerator relationship matrix is always the same. 

h2=0.5


S<-GBlup_predict(G=G_4,y=y_4,h2=h2,make_predictions=c(354:503),ntraits=1)
SR<-GBlup_predict(G=R,y=y_4,h2=h2,make_predictions=c(354:503),ntraits=1)
#Genomic PA
S$rhoM
```

    ## [1] 0.3561936

``` r
#Numerator Relationship PA
SR$rhoM
```

    ## [1] 0.30277

#### Strawberry complete dataset

The complete GBS data set is in `Data_strawberry` folder in GitHub (<https://github.com/lauzingaretti/pSBVB>).

**Citation**

L. Zingaretti, A. Monfort, M. Pérez-Enciso. 2018. pSBVB: a versatile simulation tool to evaluate genomic selection in polyploid species. Manuscript submitted for publication.

Pérez-Enciso, M., Forneris, N., de los Campos, G., & Legarra, A. (2016). Evaluating sequence-based genomic prediction with an efficient new simulator. Genetics, genetics-116.

Appendix
========

### full list of options in parameter file

`NTRAIT` \#--&gt; specifies no. of traits (int, \[0\])

`PLOIDY` \#--&gt; specifies ploidy (int, \[2\])

`QTLFILE` !--&gt; file with qtl posns (chr& bp) add &dom effects can be defined in cols 3 & 4 (str)

`SNPFILE` !--&gt; file with genotyped snps: chr, bp, can be repeated

`MAPFILE` !--&gt;recomb map file: chr, basepos, cm2Mb \[cm2Mb\_sex2\]

`HAPFILE` !--&gt; hap structure so program can be restarted with RESTART

`OUTPLINK` !--&gt; prints mkr in plink tpedformat

`OUTGFILE` !--&gt; GRM outfile

`OUTQFILE` !--&gt; output qtl file out\_q\_file

`OUTYFILE` !--&gt; y outfile outyfile

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
