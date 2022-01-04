library(dada2)

path <- "~/projects/def-lpenacas/CanineGut/Fastq_files/2_trimmed_fastq_files"


files <- function(pattern) {
    sort(list.files(path, pattern, full.names = TRUE))
}
files_f <- files("_R1_paired_001.fastq.gz")
files_r <- files("_R2_paired_001.fastq.gz")

sample_names <- sapply(strsplit(basename(files_f), "_"), "[", 1)
dada2::plotQualityProfile(files_f[1:2])

filtered_path <- function(pattern) {
    fp <- file.path(path, "dada2filter", paste0(sample_names, pattern))
    names(fp) <- sample_names
    fp
}
filtered_path_f <- filtered_path("_F_filt.fastq.gz")
filtered_path_r <- filtered_path("_R_filt.fastq.gz")

out <- dada2::filterAndTrim(
    files_f,
    filtered_path_f,
    files_r,
    filtered_path_r,
    truncLen = c(274, 230),
    maxN = 0,
    maxEE = c(2, 2),
    truncQ = 2,
    rm.phix = TRUE,
    compress = TRUE,
    multithread = TRUE
)

my_dada <- function(fp) {
    err <- dada2::learnErrors(fp, multithread = TRUE)
    dada2::plotErrors(err, nominalQ = TRUE)
    dada2::dada(filtered_path_f, , err, multithread = TRUE)
}
dada_f <- my_dada(filtered_path_f)
dada_r <- my_dada(filtered_path_r)

mergers <- dada2::mergePairs(dada_f,
    filtered_path_f,
    dada_r,
    filtered_path_r,
    verbose = TRUE
)

head(mergers[[1]])

seqtab <- dada2::makeSequenceTable(mergers)
table(nchar(getSequences(seqtab)))

seqtab.nochim <- dada2::removeBimeraDenovo(
    seqtab,
    method = "consensus",
    multithread = TRUE,
    verbose = TRUE
)


count_unique <- function(x) sum(getUniques(x))
track <- cbind(
    out,
    sapply(dada_f, count_unique),
    sapply(dada_r, count_unique),
    sapply(mergers, count_unique),
    rowSums(seqtab.nochim)
)
colnames(track) <- c(
    "input",
    "filtered",
    "denoisedF",
    "denoisedR",
    "merged",
    "nonchim"
)
rownames(track) <- sample_names
head(track)