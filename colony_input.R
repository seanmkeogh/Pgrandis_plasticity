library(sequoia)
library(radiator)
library(dartR)
library(snpStats)
library(tidyverse)

#First generate VCF with biallelic, unlinked, and no monomorphic sites on vcf file
#RUN in Terminal ->
# vcftools --vcf file.vcf --min-alleles 2 --max-alleles 2 --recode --recode-INFO-all --out file_bi
# vcftools --vcf file_bi.recode.vcf --thin 5000 --recode --recode-INFO-all --out file_bi_biunlink
# vcftools --vcf pgran88_biunlink.recode.vcf --max-missing 0.90 --recode --recode-INFO-all --out pgran88_biunlink_callrate
# vcftools --vcf pgran88_biunlink_callrate.recode.vcf --maf 0.03 --recode --recode-INFO-all --out pgran88_biunlink_callrate_maf
# 355 snps

# read in vcf
setwd('~/Box/Myprojects/Pgrandis/data/molecular/RAD_seq/colony/')
data <- radiator::read_vcf(data = "pgran88_biunlink_callrate_maf.recode.vcf")

radiator::write_colony(data = data)
write_colony(data, inbreeding=0, mating.sys.males = 1, mating.sys.females = 0,
             clone=1, run.length = 1, analysis = 1,)
