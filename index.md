
### Purpose

Polyploid sequence based virtual breeding (**pSBVB**) is a modification of **SBVB** software (Pérez-Enciso et al. 2017) that allows simulating traits of an arbitrary genetic complexity in polyploids. Its goal is to simulate complex traits and genotype data starting with a `vcf` file that contains the genotypes of founder individuals and following a given pedigree. The main output are the genotypes of all individuals in the pedigree and/or molecular relationship matrices (GRM) using all sequence or a series of SNP lists, together with phenotype data. The program implements very efficient algorithms where only the recombination breakpoints for each individual are stored, therefore allowing the simulation of thousands of individuals very quickly. Most of computing time is actually spent in reading the `vcf` file. Future developments will optimize this step by reading and writing binary mapped files. The `vcf` file may not contain missing genotypes and is assumed to be phased. Link to full manual: <https://github.com/lauzingaretti/pSBVB/blob/master/Manual.pdf>

![General Software' View](generalview.png)

### Main features

-   Any number of traits.
-   Tool adapted to work with both, auto and allo-polyploid organisms.
-   Any number of QTNs, trait specific.
-   Any number of additive and dominant effects.
-   Can generate a correlation matrix to modelate meiosis in polyploid especies.
-   Can generate correlated allelic effects and frequencies.
-   Efficient algorithms to generate haplotypes and sample SNP genotypes.
-   Computes genomic relationship matrices for any number of SNP arrays simultaneously.
-   It allow to compute Genomic relationship matrix in several ways.
-   Any number of chromosomes, allows for sex chromosomes and varying local recombination rates, that can be sex specific.

### Installation

:computer: The source code, manual and examples can be obtained from <https://github.com/lzingaretti/pSBVB>

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

The program requires blas libraries but these are standard in any unix or OS mac system. We have tested pSBVB only in linux with gfortran compiler; intel ifort seems not working, but gfortran in mac OS looks ok. Usage

To run: if you have a .vcf file

``` r2
 file.vcf | pSBVB -isbvb.par
```

if you have a .gen file

``` r2
 file.gen | pSBVB -isbvb.par
```

with alleles coded as *0/1*. To run the program with the same random seed:

``` r2
… | pSBVB -isbvb.par –seed iseed
```

where iseed is an integer number

The general format of the input file looks like that:

**allele1\_snp1\_ind1 allele2\_snp1\_ind1 allele3\_snp1\_ind1 ... allelep\_snp1\_ind1 allele1\_snp1\_ind2 allele2\_snp1\_ind2 allele3\_snp1\_ind2 ... allelep\_snp1\_indp**

**allele1\_snp2\_ind1 allele2\_snp2\_ind1 allele3\_snp2\_ind1 ... allelep\_snp2\_ind1 allele1\_snp2\_ind2 allele2\_snp2\_ind2 allele3\_snp2\_ind2 ... allelep\_snp2\_indp**

### Parameter file

The parameter file controls all **pSBVB** behavior. It consists of a list of sections in UPPER CASE (in any order) followed in the next line by the required data, e.g.,

**QTNFILE**

sbvb.qtl

tells the program that **QTN** specifications are in sbvb.qtl file. Comments can be mixed starting with \# or ! A full list of options in the parameter file is in Appendix 1. In the following, we list the main ones.

### Specifying genetic architecture

If more than one trait is generated, then use

**NTRAIT**

ntraits

in parameter file. Otherwise this section is not needed. **pSBVB** requires the user to provide the list of causal SNPs (**QTNs**) as specified in **QTNFILE** section. The format of the QTN file is the next:

| i\_chrom | i\_pos |
|:---------|:------:|

or

| <span style="font-size: 6pt">i\_chrom </span> | <span style="font-size: 6pt">i\_pos</span> | <span style="font-size: 6pt"> add\_eff\_Trait\_1 </span> | <span style="font-size: 6pt"> add\_eff\_Trait\_2 </span> | <span style="font-size: 6pt">... </span> | <span style="font-size: 6pt"> add\_eff\_Trait\_n </span> |
|:----------------------------------------------|:------------------------------------------:|:--------------------------------------------------------:|:--------------------------------------------------------:|:----------------------------------------:|----------------------------------------------------------|

or to additive and dominant effects: (:octocat:)

<table style="width:100%;">
<colgroup>
<col width="9%" />
<col width="9%" />
<col width="9%" />
<col width="9%" />
<col width="9%" />
<col width="9%" />
<col width="9%" />
<col width="9%" />
<col width="9%" />
<col width="9%" />
</colgroup>
<thead>
<tr class="header">
<th align="center"><span style="font-size: 6pt">i_chrom </span></th>
<th align="center"><span style="font-size: 6pt">i_pos</span></th>
<th align="center"><span style="font-size: 6pt"> add_eff_Trait_1 </span></th>
<th align="center"><span style="font-size: 6pt"> add_eff_Trait_2 </span></th>
<th align="center"><span style="font-size: 6pt">... </span></th>
<th align="center"><span style="font-size: 6pt"> add_eff_Trait_n </span></th>
<th align="center"><span style="font-size: 6pt"> dom_eff_Trait_1 </span></th>
<th align="center"><span style="font-size: 6pt"> dom_eff_Trait_2 </span></th>
<th align="center"><span style="font-size: 6pt">... </span></th>
<th align="center"><span style="font-size: 6pt"> dom_eff_Trait_n </span></th>
</tr>
</thead>
<tbody>
</tbody>
</table>

The bellow file, must separated by spaces, and where *ichr* is chromosome and *ipos* is position in base pair, *add\_eff* is additive effect, i.e the effect of homozygous alleles and *dom\_eff* is the heterozygous effect.

<span style="color:red; font-family:Georgia; font-weight: bold; background-color: #FFFF00 ">WARNING:</span> **QTN** position must coincide with one **SNP** position in the `vcf` file, otherwise it is not considered.

If QTN effects are not provided, they can be simulated specifying

**QTNDISTA**

u lower\_bound upper\_bound | n mu var | g s b

and

**QTNDISTD**

u lower\_bound upper\_bound | n mu var | g s b

in parameter file. where 'u' means effects are sampled from a uniform distribution *U* ∼ (*l**o**w**e**r*<sub>*b**o**u**n**d*</sub>, *u**p**p**e**r*<sub>*b**o**u**n**d*</sub>), 'n' from a normal distribution *N* ∼ (*m**u*, *v**a**r*) and 'g' from a gamma *γ* ∼ (*s*, *b*). For a gamma distribution, you can specify the probability p that a derived allele decreases the phenotype with:

**PSIGNQTN**

p

The default value is 50%. By default, effects are sampled independently of frequency, i.e., half effects are + and the rest are -, but it is possible to generate a correlation (rho) using the next parameter:

**RHOQA**

rho

This option can be useful to simulate past selection.

The narrow sense heritability is specified as:

**H2G**

h2

The software incorporates the broad sense heritability (using **H2G**). Only the genotypes from the base population (in the `vcf` file) are used to adjust heritability.

### Phenotyping simulations

As **pSBVB** takes ploidy into account to generate the phenotypes and incorporates several options to generate the molecular relationship matrix that are pertinent to polyploids. In a diploid organism, the phenotype for *i*-th individual can be simulated from:

$y\_{i}=\\mu + \\sum\_{j=1}^{Q} \\gamma\_{ij}\\alpha\_j + \\sum\_{j=1}^{Q} \\delta\_{ij}d\_j + \\epsilon\_i$

Where *μ* is the mean general, *α* is the additive effect of *j*-th locus, that is, half the expected difference between homozygous genotypes, *γ*<sub>*i**j*</sub> takes values -1, 0 and 1 for homozygous, heterozygous and alternative homozygous genotypes, respectively. *d*<sub>*j*</sub> is the dominance effect of *j*-th locus, and *δ*<sub>*i**j*</sub> takes value 1 if the genotype is heterozygous, 0 otherwise, and *ϵ*<sub>*i*</sub> is a normal residual. For polyploids, the phenotype of individual *i* (*y*<sub>*i*</sub>) (equivalent equation) is simulated from:

$y\_{i}=\\mu + \\sum\_{j=1}^{Q} \\eta\_{ij}\\alpha\_j + \\sum\_{j=1}^{Q} \\phi\_{ij}d\_j + \\epsilon\_{i}$

where *η*<sub>*i**j*</sub> is the number of copies of the alternative allele (coded say as 1) minus half the ploidy (*h*/2) for *j*-th locus and *i*-th individual, and *α*<sub>*j*</sub> is therefore the expected change in phenotype per copy of allele ‘1’ in the *j*-th locus. In polyploids, as many dominance coefficients as ploidy level (*h*) minus two can technically be defined. However, this results in an over-parameterized model that is of no practical use. Here instead we define the *ϕ*<sub>*i**j*</sub> parameter as the minimum number of copies of allele 1 such that the expected phenotype is *d*. By default, **pSBVB** uses *ϕ*<sub>*i**j*</sub> = 1 , that is, any genotype having at least one allele ‘1’ and ‘0’ has the expected phenotypic value *d*. You can coded *ϕ*<sub>*i**j*</sub> as any integer between 1 and *h* − 1. Finally, the residual *ϵ*<sub>*i*</sub> is sampled from a *N* ∼ (0, *v**e*), where *v**e* is adjusted given either **H2** or **H2G** using the genotypes from the base popula2tion. For multiple traits, the fields **H2** or **H2G**, **RHOQA**, and **QTLDISTA** and **QTLDISTD** must be repeated, eg, for two traits:

**H2**

0.5

0.23

**RHOQA**

0

-0.4

**QTNDISTA**

u -0.2 0.2

g 1 0.5

which means that the firts trait have a heredability of 0.5, a **RHOQA** parameter of 0 and **QTNDISTA** have an uniform distribution (0.2, 0.2) and the second trait have a heredability of 0.23, **RHOQA** parameter is −0.4 and **QTNDISTA** have a gamma distribution with parameters (1, 0.5)

### Recombination in polyploids

The ploidy level must be specified with section

PLOIDY

h

in the parameter file. By default, pSBVB assumes autopolyploidy and permits recombination between each homeolog chromosome pair with equal probability. Strict alloploidy is specified with

ALLOPLOIDY

Intermediate rates of recombination between homeolog pairs can be specified with

RHOPLOIDY

rho\_elements

where rho\_elements is a vector of hxh elements specifying the probability of recombination between i and j homeologs. The diagonal (P of recombining with itself) is set to 0 by the program.

### Pedigree file (PEDFILE)

The format is id id\_father id\_mother \[sex\]

where all ids must be consecutive integers, 0 if father or mother unknown, sex is optional (1 for males, 2 for females) and only needed if *sex* chr is specified. The number of individuals in the `vcf` file must be specified with section:

**NBASE**

nbase

in the parfile. The pedigree file must contain the first rows as

| <span style="font-weight:normal"> 1 </span> | <span style="font-weight:normal"> 0 </span> | <span style="font-weight:normal"> 0 </span> |
|---------------------------------------------|---------------------------------------------|---------------------------------------------|
| 2                                           | 0                                           | 0                                           |
| ...                                         | 0                                           | 0                                           |
| nbase                                       | 0                                           | 0                                           |

that is, those in `vcf` file are assumed to be unrelated.

### Recombination map files

By default, **pSBVB** assumes a cM to Mb ratio of 1. This ratio can be changed genomewide with **CM2MB** section in the par file. In addition, local recombination rates can be specified with the **MAPFILE** section. The mapfile takes format

**MAPFILE**

| ichr | last\_bp | local\_cm2mb |
|:----:|:--------:|:------------:|

where **local\_cm2mb** is the recombination rate between **last\_bp** and previous bound (1 bp if first segment) , or

| ichr | last\_bp | local\_cm2mbMales | local\_cm2mb\_females |
|:----:|:--------:|:-----------------:|:---------------------:|

The maximum number of chromosomes allowed by default is 23; should you require more, then section **MAXNCHR** must be included as:

**MAXNCHR**

nchrom

**pSBVB** permits sex chromosomes. The sex chromosome must be declared with **SEXCHR** section. Then, sex 1 is assumed to be the heterogametic sex, and a sex column should be present in the **PEDFILE**.

<span style="color:red; font-family:Georgia; font-weight: bold; background-color: #FFFF00 ">WARNING:</span> chromosome ids must be integer consecutive numbers, even for the sex chr if present.

### SNP files

**pSBVB** can compute the genomic relationship matrix for all sequence data (in two specific ways, see bellow), and/or specific SNP subsets to mimic different genotyping arrays. Several **SNP** lists can be analyzed in the same run repeating the **SNPFILE** section in the par file. Each **SNP** file has the same format as the QTN file, i.e., chromosome and base pair position, as idicated:

**SNPFILE**

| i\_chrom | i\_pos |
|:--------:|:------:|

if you add the command **MIMICDIPLOID** to parameter file, then Genomic relationship matrix is computed assuming than only presence or absence of the alternative allele could be known for the remaining, i.e., although the organism was polyploid, Genomic matrix is computed mimic diploid.

Output
------

The program writes some general info on the screen, and the following files:

• **OUTYFILE** format (contains phenotypes and breeding values):

| *i**d* | *y* | *a**d**d*<sub>*i*</sub>, *i* = 1, ..,*n**t**r**a**i**t**s* | (*a**d**d* + *d**o**m*)<sub>*i*</sub>, *i* = 1, ..,*n**t**r**a**i**t**s* |
|:------:|:---:|:----------------------------------------------------------:|:------------------------------------------------------------------------:|

where *a**d**d* is the first sum in equation of **pSBVB** software, shows above and *d**o**m* is the second term. For several traits, first are printed all add effects for every trait, next add+dom.

• **OUTQFILE** format (contains **QTN** info):

| *i**c**h**r* | *p**o**s* | *f**r**e**q*<sub>*b**a**s**e*</sub> | (*a**d**d*<sub>*i*</sub>) | (*d**o**m*<sub>*i*</sub>) | *i* = 1, ..,*n**t**r**a**i**t**s* |
|:------------:|:---------:|:-----------------------------------:|:-------------------------:|:-------------------------:|:---------------------------------:|

where *i**c**h**r* is chromosome, *p**o**s* is *Q**T**N* bp position, *f**r**e**q*<sub>*b**a**s**e*</sub> is frequency in `.vcf` file, freq is frequency along the pedigree, plus additive, dominant effects and add variance (2*p**q**α*<sup>2</sup>) contribution for each locus by trait.

• **OUTGFILE** format (contains GRM, one per SNPFILE plus sequence) A matrix of *n* × *n*, where *n* is the number of individuals in the pedigree. As many outgfiles as snpfiles are written with subscripts .1, .2 etc. .0 corresponds to sequence. To avoid using sequence, add **NOSEQUENCE** command in parfile.

• **OUTMFILE** format (contains genotypes for evey SNP file and sequence, in plink format optionally using **OUTPLINK** in parfile). As many outmfiles as snpfiles are written with subscripts .1, .2 etc. .0 corresponds to sequence. To avoid using sequence, **NOSEQUENCE** in parfile

Outqfile, outqtn, GRM and marker files are written only if the respective sections **OUTQFILE**, **OUTGFILE** and **OUTMFILE** appear in the `.par` file. Note in particular that **OUTMFILE** with sequence can be huge! To avoid printing sequence info, use

**NOSEQUENCE**

in par file.

<span style="color:green; font-family:Georgia; font-weight: bold; background-color: #FFEF12 ">NOTE:</span> To compress marker output, include **GZIP** option in parfile.

### Restart the program keeping the same haplotypes

Sometimes one can be interested in running the same experiment but with different genetic architectures or different **SNP** arrays. The program offers two convenient ways to do this as it may keep track of haplotypes so exactly the same genetic structure is preserved, **RESTART** and **RESTARTQTL** options in `.par` file.

1.With **RESTART**, haplotypes, phenotypes and **QTN** effects are preserved. This is useful to implement selection.

2.With **RESTARTQTN**, haplotypes are preserved but phenotypes and **QTN** effects are sampled again. **RESTARQTN** can be used to run different genetic architectures in the same haplotypes so results can be exactly comparable across models.

The program then writes a `.hap` file that contains all haplotype structure the first time is run. When **pSBVB** is called again with say another **SNPFILE**, then individuals have the same haplotypes as in previous runs and a new **GRM** can be generated with the new **SNP** file. An important application is to run selection. In fact, **pSBVB** can be run with different pedigree files and the **RESTART** option. **pSBVB** generates only new haplotypes for those individuals not in current `.hap` file. In a selection scheme, the user should add a new generation pedigree to current pedfile with the offspring of selected individuals. In the new run, **pSBVB** generates haplotypes and phenotypes for the new offspring.

<span style="color:#3358ff; font-family:Georgia; font-weight: bold; background-color: #fff933 ">IMPORTANT:</span> The `.hap` file is used only if **RESTART** is included in parfile. If no `.hap` file is present, a new one is generated the first time. You can check that **RESTART** is in use checking, e.g, that all phenotypes are the same in different runs.

<span style="color:red; font-family:Georgia; font-weight: bold; background-color: #FFFF00 ">WARNING:</span> **RESTARTQTN** is logically not suitable for selection, since effects are sampled anew in each run.

### Expanding the base population

Very often, complete sequence is available only for very few individuals. **pSBVB** implements an automatic option to generate additional individuals by randomly crossing the available ones and random breeding for a pre specified number of generations. To use this feature, the pedigree file must contain larger number of individuals with unknown parents than in the `vcf` file. For instance, assume your `vcf` file contains only four individuals and the pedfile is

|  <span style="font-weight:normal"> 1 </span>| <span style="font-weight:normal"> 0 </span> | <span style="font-weight:normal"> 0 </span> |
|--------------------------------------------:|---------------------------------------------|---------------------------------------------|
|                                            2| 0                                           | 0                                           |
|                                            3| 0                                           | 0                                           |
|                                          ...| 0                                           | 0                                           |
|                                           20| 0                                           | 0                                           |
|                                           21| 1                                           | 12                                          |
|                                          ...| ...                                         | ...                                         |

Then individuals 5-20 are generated by randomly crossing 1-4 ids, from id 21 onwards, normal pedigree gene dropping is implemented. The option in parfile is

**EXPAND\_BASEPOP**

| ntgen | nfam |
|:-----:|:----:|

which means that the new individuals are generated by crossing nfam individuals of the `vcf` file for ntgen generations.

Examples
========

### Prepare your files

To facilitate the software' usability, we have written some additional functions using `R` and `python` to generate the requested files (pedigree, qtn, chip) and to transform `vcf` file into `.gen`. We also incorporate two functions to generate the numerator relationship matrix and to performed predictive ability (PA) from GBLUP model.

### Function to create a pedigree to use as .ped file

Pedigree file is required to simulations. The number of founder individuals have to be equal or higher than the number of inviduals in .gen (or .vcf) file. The founders are those individuals whith genomic information.

We provide a R function to generate pedigree file. You can choose any number of founder, generation, individuals by generation and sex specification is optional as well.

### Example 1: Generate a pedigree file

The next pedigree file have 47 founders, 8 generations, 100 individuals of each generations from 1 to 7. The last generation have 1000 individuals. Sex is not considerer.

``` pedigree_s
source("pedigree.R")
M<-pedgenerator(100,4,c(rep(100,3),150),
   sex=FALSE,
   path="~/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/
   toy_strawberry/Additional_functions",exclude=47)
```

### Example 2: Generate a pedigree file with sex option

The next pedigree file have 47 founders, 8 generations, 100 individuals of each generations from 1 to 7. The last generation have 1000 individuals.

``` pedigreematrix
source("pedigree.R")
M<-pedgenerator(100,4,c(rep(100,3),150),sex=FALSE,
   path="~/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy_strawberry/
   Additional_functions",exclude=47)
```

Generate a pedigree based relationship matrix
---------------------------------------------

We had implemented a R function to generate a Relationship matrix to compare with genomic relationship matrix. Our function could be used to compute both, additive and dominant relationship matrices. To do the calculation, a pedigree file is needed as input. You can to use the pedigree matrix generated with pedigree.R as input.

``` relationship
source("RelationshipMatrix.R")
data<-read.table("~/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB
                 /toy_strawberry/Additional_functions/File_st.ped",header=FALSE)
#check the dimensions of dataset 

A<-RelMatrix(data,dominance=FALSE,path= "~/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy_strawberry/Additional_functions/")
```

Application examples
====================

Example: Generate strawberry simulated genotype + phenotype dataset using the previous pedigree file and several options.
-------------------------------------------------------------------------------------------------------------------------

The parameter file (see file\_1.par) incorporate three "H2G" parameters of 0.3, 0.4 and 0.5. 150 causals SNP's to simulate the phenotype. This file simulated simultaneosly three pheno types with three additive effects. The Genomic Relationship matrix is generated using Gtota, then the M values varying between 0 and 8. The others files have the next options:

-   file\_4.par --&gt; heredability of 0.5, generate 1 trait. Use GT as genomic relationship matrix. Additive and dominat effects are simulated from a gamma distribution with mean=0.2, shape= 0.2 and mean= 0.2, shape= 0.5 parameters respectively.
-   file\_5.par --&gt; heredability of 0.3, generate 1 trait. Use GT as genomic relationship matrix, 80 QTNS

You can run these models and others with files available in /toy\_strawberry folder.

You can run all files simultaneosly using run\_toy\_st.sh file. You could run the program once:

``` bash
cat toy_st.gen | sbvb -i file_1.par
```

or using a .sh file:

``` r2
./run_toy_st.sh 
```

The next images show how it works:
![Run the program from command line](example.run.png)

![Run the program several times simultaneosly using a bash script](runbash.png)

Statistical Models:
===================

There are numerous GS methods that use genome-wide markers to predict breeding values and address the large *p* small *n* problem. Here, breeding values were predicted using GBLUP, which is a genomic-based extension of traditional BLUP (Henderson, 1984) and equivalent to Ridge Regression Model (RR-BLUP). The model is:

The R script to perform Predictive abilities is GBlupFunction.R

Running Model 1
---------------

Here, we include the output from three models simultaneosly and we compared them with Pedigree Based models. Finally, we plot the estimated vs. true values. We only graph the thirth model (heritability parameters to each model were set on 0.3, 0.4 and 0.5).

In this example, we have simulated three phenotypes simultaneosly. The GBlup Prediction, could be used to evaluated predictive of those traits ability simultaneosly.

``` r
library(ggplot2)
#choose the dataset directory  using setwd() /results_example1 
# Read y and G using 
#G<- read.table("Y.grm.1")
#y<- read.table("Y.outy")

h2=c(0.3,0.4,0.5)
ntraits=3
make_predictions=c(353:503)

S<-GBlup_predict(G=G,y=y,h2=h2,make_predictions=make_predictions,ntraits=ntraits)

#Predictive ability computed with GModel 
S[[3]]$rho
```

    ## [1] 0.5372807

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

![](help_addfunctions_version2_files/figure-markdown_github/unnamed-chunk-13-1.png)

Running Model 2
===============

This model includes dominant effects from an uniform distribution with parameters a=0.2, b=0.6

``` r
source("GBlupFunction.R")
setwd("/home/laura/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy_strawberry/results_example4/")

G<- read.table("Y.grm.1")
y<- read.table("Y.outy")
#check matrix dimention
setwd("/home/laura/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy_strawberry/")
R<-read.table("Relationship.mat")
#check matrix dimention
R<-R[c(48:550),c(48:550)]
#In the example one, we had been simulated three phenotypes simultaneosly. The GBlup Prediction, could be used to evaluated predictive of those traits ability simultaneosly. 
h2=0.5
ntraits=1
make_predictions=c(354:503)

S<-GBlup_predict(G=G,y=y,h2=h2,make_predictions=c(354:503),ntraits=ntraits)
SR<-GBlup_predict(G=R,y=y,h2=h2,make_predictions=c(354:503),ntraits=ntraits)
##gENETIC MODEL PA
S$rhoM
```

    ## [1] 0.4299901

``` r
#RELATIONSHIP MODEL PA
SR$rhoM
```

    ## [1] 0.5142909

Example with MIMIC\_DIPLOID activate
====================================

This example includes an heritability parameter of 0.5, 150 QTN's effects from the 1500 available ones. The Genetic matrix was assesed asuming Diploid.

``` r
source("GBlupFunction.R")
setwd("/home/laura/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy_strawberry/results_example2/")

####with mimic diploid option

G<- read.table("Y.grm.1")
y<- read.table("Y.outy")
#check matrix dimention
dim(y)
```

    ## [1] 503   5

``` r
dim(G)
```

    ## [1] 503 503

``` r
setwd("/home/laura/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy_strawberry/")

R<-read.table("Relationship.mat")
#check matrix dimention
dim(R)
```

    ## [1] 550 550

``` r
R<-R[c(48:550),c(48:550)]
h2=0.5
ntraits=1
make_predictions=c(354:503)

S<-GBlup_predict(G=G,y=y,h2=h2,make_predictions=c(354:503),ntraits=1)
SR<-GBlup_predict(G=R,y=y,h2=h2,make_predictions=c(354:503),ntraits=1)
#correlation with Genetic Matrix
S$rhoM
```

    ## [1] 0.5116903

``` r
#correlation with Numerator relationship matrix
SR$rhoM
```

    ## [1] 0.4031313

Example with MIMIC\_HAPLOID
===========================

This example includes an heritability parameter of 0.5, 150 QTN's effects from the 1500 available ones. The Genetic matrix was assesed using MIMIC\_HAPLOID option.

``` r
source("GBlupFunction.R")
setwd("/home/laura/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy_strawberry/results_example8/")
####with mimic haploid option 
G<- read.table("Y.grm.1")
y<- read.table("Y.outy")
setwd("/home/laura/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy_strawberry/")

R<-read.table("Relationship.mat")
#check matrix dimention
R<-R[c(48:550),c(48:550)]
h2=0.5
make_predictions=c(354:503)

S<-GBlup_predict(G=G,y=y,h2=h2,make_predictions=c(354:503),ntraits=1)
SR<-GBlup_predict(G=R,y=y,h2=h2,make_predictions=c(354:503),ntraits=1)
#Genomic PA
S$rhoM
```

    ## [1] 0.4807767

``` r
#Numerator Relationship PA
SR$rhoM
```

    ## [1] 0.4241474

Examples with potato dataset:
=============================

``` r
setwd("/home/laura/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy_strawberry/Additional_functions/")
source("pedigree.R")
source("RelationshipMatrix.R")
```

    ## Completed! Time = 0.00125  minutes

``` r
source("GBlupFunction.R")
source("generate_vcf_from_gen.R")
```

We also created a function to transformate genotypes into 'vcf' format. To do that, you need a dataset with genotypes (data varying between 0 and ploidy level) and a map file, containing the SNP's physical coordinates. We used genotypes from potato database (<https://figshare.com/articles/Supplemental_Material_for_Enciso-Rodriguez_et_al_2018/6262214>). Note that, our function generate the vcf file randomly from genotype. In order to generate Linkage Disequilibrium, we could combine two pSBVB parameters: EXPAND\_BASEPOP and INDFIRST, which simultaneosly exclude the initial individuals and generate a new set of founder.

Furthermore, the toy\_potato/FilesGenerator.R file have all the functions needed to create a Chip, pedigree, QTNs location files and run potato example.

``` r
setwd("/home/laura/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy_potato/")
#read map file 
map<-read.table("Sol_tet_mapa.map",sep="\t")
#read genotype
G<-read.delim("Pot_gen.gen",sep="\t")
#select a sub-sample from whole dataset 
G=G[sample(c(1:nrow(G)),150),]
G=G[,sample(c(1:ncol(G)),500)]
G<-G[,colnames(G)%in%map[,1]]
dim(G)
```

    ## [1] 150 395

``` r
#generate genotypes vcf format 
A=GenotoVcf(G,p=4,map,path="NULL")
```

    ## Loading required package: data.table

``` r
library(ggplot2)
setwd("/home/laura/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy_strawberry/Additional_functions/")
source("GBlupFunction.R")

data<-read.table("/home/laura/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy_potato/File_st.ped",header=FALSE)
#check the dimensions of dataset 
#generate relationship matrix
dim(data)
```

    ## [1] 700   3

``` r
A<-RelMatrix(data,dominance=FALSE,path="/home/laura/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy_potato/")
```

    ## Completed! Time = 0.002016667  minutes

``` r
setwd("/home/laura/Dropbox/DoctoradoCRAG/paper-1/article/software-help/reversinn1/pSBVB/toy_potato/results_potato/")

G<- read.table("Y.grm.1")
y<- read.table("Y.outy")
RelMatrix<-A[-c(1:150),-c(1:150)]


h2=0.5
ntraits=1

make_predictions=c(401:550)
library(ggplot2)
S<-GBlup_predict(G=G,y=y,h2=h2,make_predictions=make_predictions,ntraits=ntraits)
S$rho
```

    ## [1] 0.511987

``` r
Phenohat<-S$uhat[make_predictions] + S$mean
Pheno<-S$yout_corr[make_predictions]
cor(Pheno,Phenohat)
```

    ## [1] 0.511987

``` r
datos<-data.frame(Phenohat,Pheno)
colnames(datos)<-c("Yhat","Y")
apply(datos,2,class)
```

    ##      Yhat         Y 
    ## "numeric" "numeric"

``` r
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

![](help_addfunctions_version2_files/figure-markdown_github/unnamed-chunk-19-1.png)

``` r
make_predictions=c(401:550)


SR<-GBlup_predict(G=RelMatrix,y=y,h2=h2,make_predictions=c(401:550),ntraits=ntraits)
SR$rhoM
```

    ## [1] 0.3104688

``` r
Pheno<-SR$yout_corr[make_predictions]
datos_r<-data.frame(Phenohat,Pheno)
colnames(datos_r)<-c("Yhat","Y")

#Pedigree Prediction 
ggplot(datos_r, aes(y=Yhat, x=Y,colour="red")) +
  geom_point(size=0.6) + scale_shape_manual(values=c(2,4)) + 
  geom_abline(intercept=lm(Yhat ~ Y,data=datos_r)$coefficients[1], slope=lm(Yhat ~ Y,data=datos_r)$coefficients[2])+
   ggtitle("Estimated values for testing set - N Relationsip matrix")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position ="none")
```

![](help_addfunctions_version2_files/figure-markdown_github/unnamed-chunk-19-2.png)

**Citation** L. Zingaretti, A. Monfort, M. Pérez-Enciso. 2018. pSBVB: a versatile simulation tool to evaluate genomic selection in polyploid species. Manuscript submitted for publication. M. Pérez-Enciso, N. Forneris, G. de los Campos, A. Legarra. An evaluation of sequence-based genomic prediction in pigs using an efficient new simulator. Submitted.

Appendix
--------

`NTRAIT`
ntrait

`PLOIDY`

P

`MAXNCHR` !—&gt; max no. of chromosomes \[23\] maxnchr

`SEXCHR` !--&gt; chr id (number) of sex chromosome, ! males(sex=1) are assumed to be the heterogametic sex, chr Y is not considered

sexchr

`QTLFILE` !--&gt; file with qtl posns (chr& bp) add &dom effects can be defined in cols 3 & 4 qtlfile

`EPIFILE` epifile

`PEDFILE` pedfile

`SNPFILE` !--&gt; file with genotyped snps: chr, bp, can be repeated snpfile

`MAPFILE` !--&gt;recomb map file: chr, basepos, cm2Mb \[cm2Mb\_sex2\] mapfile

`HAPFILE` !--&gt; hap structure so program can be restarted with RESTART hapfile

`OUTPLINK` !--&gt; prints mkr in plink tpedformat

`OUTGFILE` !--&gt; GRM outfile outgfile

`OUTQFILE` !--&gt; output qtl file out\_q\_file

`OUTYFILE` !--&gt; y outfile outyfile

`OUTMFILE` !--&gt; output file with mkr data

outmfile

`GZIP` !--&gt; compress output files

`NBASE` !--&gt;nind which genotypes are read from STDIN

nbase

`H2G` !--&gt; broad heritability h2g ! repeated if multiple traits

`RHOQA` !--&gt; desired correlation between allele effect and frequency rhoqa ! repeated if multiple traits

`SIGNQTN` !--&gt; P of derived allele being deleterious (only with gamma) \[0.5\] p\_sign\_qtl

`QTLDISTA` !--&gt; QTL add effects are sampled from a distribution: u(niform), g(amma), n(ormal) \[u, l\_bound, u\_bound\] | \[n, mu, var\] | \[g, s, b\] ! repeated if multiple traits

`QTLDISTD` !--&gt; QTL dom effects are sampled from a distribution \[u, l\_bound, u\_bound\] | \[n, mu, var\] | \[g, s, b\] ! repeated if multiple traits

`CM2MB` !--&gt; cM to Mb rate, default cm2mb \[1.0\] cm2mb

`MXOVER` !--&gt; Max no xovers, default 3 mxover

`RESTART` !--&gt; prepares files for new run of sbvb

`RESTARTQTL` !--&gt; restart qtl effects but keeps haplotype structure

`NOPRINTHAP` !--&gt; does not print hap file, eg, if no new haplotypes have been generated

`NOSEQUENCE` !--&gt; does not use sequence for GRM,

`EXPAND_BASEPOP` !--&gt; breeds new base individuals involving random mating for ntgen generations ! from nfam families

ntgen nfam

`ALLOPOLYPLOID` !--&gt; if PLOIDY is higher than 2 and you want to simulate an allopolyploid organism.

`MIMIC_DIPLOID` !--&gt; this option assumes than only presence or absence of the alternative allele can be ascertained for genotypes’ values higher than 2 (only should be on if organism is polyploid)

`MIMIC_HAPLOID` !--&gt; Assuming that only one allele can be distinguished for the others, i.e., that a given marker allele behaves as fully dominant.
