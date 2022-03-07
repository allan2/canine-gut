source("taxa.R")
source("find_hyperparams.R")

family_keep <- c("Bacteroidaceae", "Oscillospiraceae", "Ruminococcaceae")
genus_keep <- c("Blautia", "Faecalibacterium", "Bacteroides")

# Prep
df <- read.table("data/SeqTab_NoChim_SamplesInColumns_in24Dogs.tsv",
    sep = "\t", header = TRUE, row.names = NULL
)
colnames(df)[1] <- "seq"
# Suturella gets removed when we filter on family and genus anyway.
taxa <- seq_taxa(keep_suterella = TRUE)
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

# df_anx <- df[sample(nrow(df_anx), 200), ]
rm(df)

set.seed(1)
n_trees <- seq(100, 500, 100)
n_col <- ncol(df_anx)
m_values <- c(log(n_col), sqrt(n_col), (n_col / 4))
ctrl <- train_ctrl()

# Anxiety
models <- list()
idx <- 0
for (m in m_values) {
    idx <- idx + 1
    # Use the entire data set because we have few dogs.
    tune_grid <- expand.grid(.mtry = m)
    for (n_tree in n_trees) {
        start_time <- Sys.time()
        fit <- train_anxiety(df_anx, n_tree, tune_grid)
        end_time <- Sys.time()
        elapsed <- end_time - start_time
        print(elapsed)
        models[[toString(n_tree)]] <- fit
        res <- data.frame(fit$results)
        filename <- paste0("output_anx_fam", idx, ".csv")
        write.table(res,
            filename,
            header = FALSE,
            append = FALSE,
            sep = ",",
            row.names = TRUE,
            col.names = TRUE
        )
    }
}