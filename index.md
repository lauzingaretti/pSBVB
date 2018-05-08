
where all ids must be consecutive integers, $0$ if father or mother unknown, sex is optional ($1$ for males, $2$ for females) and only needed if _sex_ chr is specified. The number of individuals in the ```vcf``` file must be specified with section:

**NBASE**

 nbase

 in the parfile. The pedigree file must contain the first rows as

| <span style="font-weight:normal"> 1 </span>  |  <span style="font-weight:normal"> 0 </span>  |  <span style="font-weight:normal"> 0 </span> |
|---|---|---|
|   2 | 0 | 0 |
| ... | 0 | 0 |
| nbase | 0 | 0 |

that is, those in ```vcf``` file are assumed to be unrelated.

### Recombination map files
By default, **pSBVB** assumes a cM to Mb ratio of 1. This ratio can be changed genomewide with **CM2MB** section in the par file. In addition, local recombination rates can be specified with the **MAPFILE** section. The mapfile takes format

**MAPFILE**

| ichr | last_bp | local_cm2mb |
| :--: | :-----: | :----------:|

where **local_cm2mb** is the recombination rate between **last_bp** and previous bound (1 bp if first segment) , or

| ichr | last_bp | local_cm2mbMales | local_cm2mb_females |
|:----:|:-------:|:----------------:|:-------------------:|



The maximum number of chromosomes allowed by default is 23; should you require more, then section **MAXNCHR** must be included as:

**MAXNCHR**

nchrom

**pSBVB** permits sex chromosomes. The sex chromosome must be declared with **SEXCHR** section. Then, sex 1 is assumed to be the heterogametic sex, and a sex column should be present in the **PEDFILE**.

<span style="color:red; font-family:Georgia; font-weight: bold; background-color: #FFFF00 ">WARNING:</span> chromosome ids must be integer consecutive numbers, even for the sex chr if present.

### SNP files
**pSBVB** can compute the genomic relationship matrix for all sequence data (in two specific ways, see bellow), and/or specific SNP subsets to mimic different genotyping arrays. Several **SNP** lists can be analyzed in the same run repeating the **SNPFILE** section in the par file. Each **SNP** file has the same format as the QTN file, i.e., chromosome and base pair position, as idicated:

**SNPFILE**

| i_chrom | i_pos |
|:-------:|:-----:|

if you add the command **MIMICDIPLOID**  to parameter file, then Genomic relationship matrix is computed assuming than  only presence or absence of the alternative allele could be known for the remaining, i.e., although the organism was polyploid, Genomic matrix is computed mimic diploid.

## Output

The program writes some general info on the screen, and the following files:


•	**OUTYFILE** format (contains phenotypes and breeding values):

| $id$ | $y$ | $add_i , i=1,..,ntraits$ | $(add+ dom)_i, i=1,..,ntraits$|
|:---: | :--:| :--------:| :--------------:|

where $add$ is the first sum in equation of **pSBVB** software, shows above and $dom$ is the second term. For several traits, first are printed all add effects for every trait, next add+dom.

•	**OUTQFILE** format (contains **QTN** info):

| $ichr$| $pos$ | $freq_{base}$ | $(add_i)$| $(dom_i)$| $i=1,..,ntraits$|
| :---: | :---: | :-----------: | :-------: | :-------: | :-------: |


where $ichr$ is chromosome, $pos$ is $QTN$ bp position, $freq_{base}$ is frequency in ```.vcf``` file, freq is frequency along the pedigree, plus additive, dominant effects and add variance $(2pq\alpha^2)$ contribution for each locus by trait.

•	**OUTGFILE** format (contains GRM, one per SNPFILE plus sequence)
A matrix of $n × n$, where $n$ is the number of individuals in the pedigree.  As many outgfiles as snpfiles are written with subscripts .1, .2 etc. .0 corresponds to sequence. To avoid using sequence, add  **NOSEQUENCE** command in parfile.

•	**OUTMFILE** format (contains genotypes for evey SNP file and sequence, in plink format optionally using **OUTPLINK** in parfile). As many outmfiles as snpfiles are written with subscripts .1, .2 etc. .0 corresponds to sequence. To avoid using sequence, **NOSEQUENCE** in parfile

Outqfile, outqtn, GRM and marker files are written only if the respective sections **OUTQFILE**, **OUTGFILE** and **OUTMFILE** appear in the ```.par``` file. Note in particular that **OUTMFILE** with sequence can be huge! To avoid printing sequence info, use

**NOSEQUENCE**

in par file.

<span style="color:green; font-family:Georgia; font-weight: bold; background-color: #FFEF12 ">NOTE:</span> To compress marker output, include **GZIP** option in parfile.

### Restart the program keeping the same haplotypes

Sometimes one can be interested in running the same experiment but with different genetic architectures or different **SNP** arrays. The program offers two convenient ways to do this as it may keep track of haplotypes so exactly the same genetic structure is preserved, **RESTART** and **RESTARTQTL** options in ```.par``` file.

1.With **RESTART**, haplotypes, phenotypes and **QTN** effects are preserved. This is useful to implement selection.

2.With **RESTARTQTN**, haplotypes are preserved but phenotypes and **QTN** effects are sampled again. **RESTARQTN** can be used to run different genetic architectures in the same haplotypes so results can be exactly comparable across models.

The program then writes a ```.hap``` file that contains all haplotype structure the first time is run. When **pSBVB** is called again with say another **SNPFILE**, then individuals have the same haplotypes as in previous runs and a new **GRM** can be generated with the new **SNP** file. An important application is to run selection. In fact, **pSBVB** can be run with different pedigree files and the **RESTART** option. **pSBVB** generates only new haplotypes for those individuals not in current ```.hap``` file. In a selection scheme, the user should add a new generation pedigree to current pedfile with the offspring of selected individuals. In the new run, **pSBVB** generates haplotypes and phenotypes for the new offspring.

<span style="color:#3358ff; font-family:Georgia; font-weight: bold; background-color: #fff933 ">IMPORTANT:</span>
The ```.hap``` file is used only if **RESTART** is included in parfile. If no ```.hap``` file is present, a new one is generated the first time. You can check that **RESTART** is in use checking, e.g, that all phenotypes are the same in different runs.

<span style="color:red; font-family:Georgia; font-weight: bold; background-color: #FFFF00 ">WARNING:</span>
**RESTARTQTN** is logically not suitable for selection, since effects are sampled anew in each run.

### Expanding the base population
Very often, complete sequence is available only for very few individuals. **pSBVB** implements an automatic option to generate additional individuals by randomly crossing the available ones and random breeding for a pre specified number of generations. To use this feature, the pedigree file must contain larger number of individuals with unknown parents than in the ```vcf``` file. For instance, assume your ```vcf``` file contains only four individuals and the pedfile is


| <span style="font-weight:normal"> 1 </span> | <span style="font-weight:normal"> 0 </span> | <span style="font-weight:normal"> 0 </span> |
| -------------------------------------------:| ------------------------------------------- | ------------------------------------------- |
|                                           2 | 0                                           | 0                                           |
|                                           3 | 0                                           | 0                                           |
|                                         ... | 0                                           | 0                                           |
|                                          20 | 0                                           | 0                                           |
|                                          21 | 1                                           | 12                                          |
|                                ...          |  ...                                        | ...                                         |



Then individuals 5-20 are generated by randomly crossing 1-4 ids, from id 21 onwards, normal pedigree gene dropping is implemented. The option in parfile is

**EXPAND_BASEPOP**

| ntgen| nfam |
| :---:| :--: |


which means that the new individuals are generated by crossing nfam individuals of the ```vcf``` file for ntgen generations.

### Examples

Folder Examples contains two, a toy example consisting in three **SNPs** from a tetraploid individuals and one example consisting of 150 SNPs from the X chromosome of 100 lines from octoploid strawberry lines. The description of the files is:

**Base genotypes**
test.vcf: original vcf file
test.gen: results from cat test.vcf | perl vcf2tped2.pl -hap | cut -d ' ' -f 1,4-

One trait (cat test.gen | sbvb -i test.par)
test.par: par fisigmale
test.qtn: list of causal SNPs, additive effects are sampled from a gamma
test.epi: epi file with two interactions (commented out in test.par by default)
test.chip: a list of SNPs from a given array
test.outy: phenotype and breeding values
test.outq: QTN effects
test.outm* : genotypes data
test.outg: GRMs

Two traits (cat test.gen | sbvb -i test.par)
test2.par
test2.qtnsigma
test2.epi
test2.outq
...

**Citation**
M. Pérez-Enciso, N. Forneris, G. de los Campos, A. Legarra. An evaluation of sequence-based genomic prediction in pigs using an efficient new simulator. Submitted.

## Appendix
