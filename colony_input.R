library(sequoia)
library(radiator)
library(dartR)
library(snpStats)
library(tidyverse)

#First generate VCF with biallelic, unlinked, and no monomorphic sites on vcf file
#RUN in Terminal ->
# vcftools --vcf file.vcf --min-alleles 2 --max-alleles 2 --recode --recode-INFO-all --out file_bi
# vcftools --vcf file_bi.recode.vcf --thin 5000 --recode --recode-INFO-all --out file_bi_biunlink
# vcftools --vcf pgran88_biunlink.recode.vcf --max-missing 0.90/0.80/0.70 --recode --recode-INFO-all --out pgran88_biunlink_callrate
# vcftools --vcf pgran88_biunlink_callrate.recode.vcf --maf 0.03 --recode --recode-INFO-all --out pgran88_biunlink_callrate_maf
# 355/2247/4654/---9241 snps

# read in max-missing 0.70 vcf
setwd('~/Box/Myprojects/Pgrandis/data/molecular/RAD_seq/colony/70')
data <- radiator::read_vcf(data = "pgran88_biunlink_callr7_maf.recode.vcf")

radiator::write_colony(data = data)
write_colony(data, inbreeding=0, mating.sys.males = 1, mating.sys.females = 0,
             clone=1, run.length = 1, analysis = 1,)

# Need this to relabel loci from radiator format
snps<-seq(from=1, to =4654, by =1)
loci<-NULL
for (i in snps){
  loci<-append(loci, paste0('Locus_',snps[i]))
}
write_txt(loci, "loci_names.txt", line_glue = ' ')

loci_df<-data.frame(matrix(NA, nrow = 3, ncol = 4654))
names(loci_df)<-loci
loci_df[1,]<-0
loci_df[2,]<-0
loci_df[3,]<-0.2
write.table(loci_df, "MarkerTypeErrorRate.txt", append = FALSE, sep = " ", dec = ".",
            row.names = FALSE, col.names = TRUE,quote = FALSE)
loci_df

# read in max-missing 0.80 vcf
setwd('~/Box/Myprojects/Pgrandis/data/molecular/RAD_seq/colony/80')
data <- radiator::read_vcf(data = "pgran88_biunlink_callr8_maf.recode.vcf")

radiator::write_colony(data = data)
write_colony(data, inbreeding=0, mating.sys.males = 1, mating.sys.females = 0,
             clone=1, run.length = 1, analysis = 1,)

# Need this to relabel loci from radiator format
snps<-seq(from=1, to =2247, by =1)
loci<-NULL
for (i in snps){
  loci<-append(loci, paste0('Locus_',snps[i]))
}
write_txt(loci, "loci_names.txt", line_glue = ' ')

loci_df<-data.frame(matrix(NA, nrow = 3, ncol = 2247))
names(loci_df)<-loci
loci_df[1,]<-0
loci_df[2,]<-0
loci_df[3,]<-0.2
write.table(loci_df, "MarkerTypeErrorRate.txt", append = FALSE, sep = " ", dec = ".",
            row.names = FALSE, col.names = TRUE,quote = FALSE)
