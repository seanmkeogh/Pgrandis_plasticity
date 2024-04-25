library(dartR)
library(snpStats)
library(tidyverse)
library(mclm)
setwd('~/Box/myprojects/Pgrandis/data/molecular/RAD_seq/dartR/')

#First generate VCF with biallelic, unlinked, and no monomorphic sites on vcf file
#RUN in Terminal ->
#RUN in Terminal ->
# vcftools --vcf file.vcf --min-alleles 2 --max-alleles 2 --recode --recode-INFO-all --out file_bi
# vcftools --vcf file_bi.recode.vcf --thin 5000 --recode --recode-INFO-all --out file_bi_biunlink
# vcftools --vcf pgran88_biunlink.recode.vcf --max-missing 0.90/0.80/0.70 --recode --recode-INFO-all --out pgran88_biunlink_callrate
# vcftools --vcf pgran88_biunlink_callrate.recode.vcf --maf 0.03 --recode --recode-INFO-all --out pgran88_biunlink_callrate_maf

# read in vcf
gl_usnps_all <- dartR::gl.read.vcf("pgran88_biunlink_callrate_maf.recode.vcf")
dartR::gl.compliance.check(gl_usnps_all, verbose=5)

sampling <- read_tsv("strata.txt", col_names = TRUE)

#add populations
strata(gl_usnps_all) <- sampling %>% 
  select(sample_ID, pop)

setPop(gl_usnps_all) <- ~pop
popNames(gl_usnps_all) # should be 5

# List group numbers
table(pop(gl_usnps_all))

#test to make sure order is correct
cbind.data.frame(gl_usnps_all$ind.names,gl_usnps_all$pop)

#compliance check
dartR::gl.compliance.check(gl_usnps_all, verbose=5)

#now run pairwise Fst
fst_results<-gl.fst.pop(gl_usnps_all,nboots=100,percent=95,nclusters=2,verbose=3)

fst_results$Fsts
#########################
## Now do the same thing just between habitat types (lake vs. stream)
#########################

# read in vcf
gl_usnps_all <- dartR::gl.read.vcf("pgran88_biunlink_callrate_maf.recode.vcf")
dartR::gl.compliance.check(gl_usnps_all, verbose=5)

sampling <- read_tsv("habitat.txt", col_names = TRUE)

#add populations
strata(gl_usnps_all) <- sampling %>% 
  select(sample_ID, pop)

setPop(gl_usnps_all) <- ~pop
popNames(gl_usnps_all) # should be 5

# List group numbers
table(pop(gl_usnps_all))

#test to make sure order is correct
cbind.data.frame(gl_usnps_all$ind.names,gl_usnps_all$pop)

#compliance check
dartR::gl.compliance.check(gl_usnps_all, verbose=5)

#now run pairwise Fst
fst_results<-gl.fst.pop(gl_usnps_all,nboots=100,percent=95,nclusters=2,verbose=3)

fst_results$Fsts
