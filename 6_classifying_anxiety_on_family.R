source("taxa.R")
source("find_hyperparams.R")

family_keep <- c("Bacteroidaceae", "Oscillospiraceae", "Ruminococcaceae")
genus_keep <- c("Blautia", "Faecalibacterium", "Bacteroides")

# Prep
df <- read.table("data/SeqTab_NoChim_SamplesInColumns_in24Dogs.tsv",
    sep = "\t", header = TRUE, row.names = NULL
)
colnames(df)[1] <- "seq"
taxa <- seq_taxa()
# Filter taxa to keep only the families and genuses we want.
taxa <- taxa[
    (taxa$Family %in% family_keep |
        taxa$Genus %in% genus_keep),
]
# Inner join on the sequence ID (the index).
df_a <- merge(df, taxa, by = "seq")

# Transpose so that OTUs are columns. The index is "SampleN".
df <- as.data.frame(t(df))
# We don't need the full sequence as the column names.
# Replace them with numbers.
colnames(df) <- seq_len(length(colnames(df)))

# The index is "SampleN".
sample_vars <- read.csv("data/samplevariables.csv",
    header = TRUE, row.names = 1
)

# Only keep the two classifiers.
colnames(sample_vars) <- tolower(colnames(sample_vars))
sample_vars <- sample_vars[, names(sample_vars) %in% c(
    "anxiety", "aggression"
)]

# Left join.
# We now have a data frame with sample, read_count, anxiety, and aggression.
df <- merge(x = df, y = sample_vars, all.x = TRUE)

# Convert classifiers to factors.
df$anxiety <- factor(df$anxiety)
# Remove the aggression column.
df_anx <- subset(df, select = -c(aggression))

# Sample
# df_anx <- df_anx[sample(nrow(df_anx), 200), ]
rm(df)

set.seed(1)
run("anx_fam", df_anx, ctrl, train_anxiety)