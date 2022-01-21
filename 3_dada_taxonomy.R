## https://github.com/grimmlab/MicrobiomeBestPracticeReview/blob/master/Amplicon_analysis/dada2_workflow.R
library(dada2)
packageVersion("dada2")

path <- "~/projects/def-lpenacas/CanineGut/Fastq_files/2_trimmed_fastq_files"
list.files(path)
fnFs <- sort(list.files(path, pattern = "_R1_paired_001.fastq.gz", full.names = TRUE))
fnRs <- sort(list.files(path, pattern = "_R2_paired_001.fastq.gz", full.names = TRUE))
sample.names <- sapply(strsplit(basename(fnFs), "_"), `[`, 1)
png("QualityProfiles_forward.png")
plotQualityProfile(fnFs[1:2])
dev.off()
png("QualityProfiles_reverse.png")
plotQualityProfile(fnRs[1:2])
dev.off()
filtFs <- file.path(path, "dada2filter", paste0(sample.names, "_F_filt.fastq.gz"))
filtRs <- file.path(path, "dada2filter", paste0(sample.names, "_R_filt.fastq.gz"))
names(filtFs) <- sample.names
names(filtRs) <- sample.names

# Filter and trim
out <- filterAndTrim(fnFs, filtFs, fnRs, filtRs, truncLen = c(274, 230), maxN = 0, maxEE = c(2, 2), truncQ = 2, rm.phix = TRUE, compress = TRUE, multithread = TRUE)
head(out)
write.csv(out, file = "out_dada2.csv", append = FALSE, sep = " ", dec = ".", row.names = TRUE, col.names = TRUE)

# Learn the error rates
errF <- learnErrors(filtFs, multithread = TRUE)
errR <- learnErrors(filtRs, multithread = TRUE)
sink("errF.txt")
print(summary(errF))
sink()
sink("errR.txt")
print(summary(errR))
sink()
png("dada2_errors.png")
plotErrors(errF, nominalQ = TRUE)
plotErrors(errR, nominalQ = TRUE)
dev.off()

################
# Dereplication
################
derepFs <- derepFastq(filtFs, verbose = TRUE)
derepRs <- derepFastq(filtRs, verbose = TRUE)
# Name the derep-class objects by the sample names
names(derepFs) <- sample.names
names(derepRs) <- sample.names


###################
# Sample Inference
###################
dadaFs <- dada(derepFs, err = errF, multithread = TRUE)
dadaRs <- dada(derepRs, err = errR, multithread = TRUE)
sink("dadaFs.txt")
print(summary(dadaFs))
sink()
sink("dadaRs.txt")
print(summary(dadaRs))
sink()
dadaFs[[1]]
dadaRs[[1]]
sink("dadaFs_2.txt")
print(summary(dadaFs[[1]]))
sink()
sink("dadaRs_2.txt")
print(summary(dadaRs[[1]]))
sink()

####################
# Merge paired reads
####################
mergers <- mergePairs(dadaFs, derepFs, dadaRs, derepRs, verbose = TRUE)
sink("mergers_successful.txt")
print(summary(mergers))
sink()
head(mergers[[1]])
write.csv(mergers[[1]], file = "mergers_dada2.csv", append = FALSE, sep = " ", dec = ".", row.names = TRUE, col.names = TRUE)

##########################
# Construct sequence table
###########################
seqtab <- makeSequenceTable(mergers)
dim(seqtab)
table(nchar(getSequences(seqtab)))

##################
# Remove chimeras
##################
seqtab.nochim <- removeBimeraDenovo(seqtab, method = "consensus", multithread = TRUE, verbose = TRUE)
dim(seqtab.nochim)
sum(seqtab.nochim) / sum(seqtab)


#################
# Save into Rds
#################
saveRDS(seqtab, "/seqtab.Rds")
saveRDS(seqtab.nochim, "/seqtab.nochim.Rds")


#################################
# rack reads through the pipeline
#################################
getN <- function(x) sum(getUniques(x))
track <- cbind(out, sapply(dadaFs, getN), sapply(dadaRs, getN), sapply(mergers, getN), rowSums(seqtab.nochim))
colnames(track) <- c("input", "filtered", "denoisedF", "denoisedR", "merged", "nonchim")
c("input", "filtered", "denoisedF", "denoisedR", "merged", "nonchim")
rownames(track) <- sample.names
head(track)
write.csv(track, file = "track_dada2.csv", append = FALSE, sep = " ", dec = ".", row.names = TRUE, col.names = TRUE)

#################
# Assign taxonomy
#################
# taxa <- assignTaxonomy(seqtab.nochim, "~/silva_nr_v132_train_set.fa.gz", minBoot = 80, outputBootstraps = TRUE, multithread=TRUE)
# taxa <- addSpecies(taxa, "~/silva_species_assignment_v132.fa.gz")
# saveRDS(taxa, "~/taxa_table_final.rds")