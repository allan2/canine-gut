library(dada2)
outdir <- "output_dada2_taxonomy_250_200"
seqtab.nochim <- readRDS(paste0(outdir, "/seqtab.nochim.Rds"))

##############
# Assign taxonomy
#################
taxa <-
    assignTaxonomy(
        seqtab.nochim,
        "silva_nr99_v138.1_train_set.fa",
        minBoot = 80,
        multithread = TRUE,
        verbose = TRUE
    )
taxa <-
    addSpecies(taxa, "silva_species_assignment_v138.1.fa", allowMultiple = TRUE)
write.table(
    taxa,
    file = paste0(outdir, "/taxa_dada2.tsv"),
    quote = FALSE,
    sep = "\t"
)