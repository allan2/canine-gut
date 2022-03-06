source("taxa.R")
library(caret)

# Reads the data into the form we want.
prep <- function(keep_suterella) {
    # Read count data.
    df <- read.table("data/SeqTab_NoChim_SamplesInColumns_in24Dogs.tsv",
        sep = "\t", header = TRUE, row.names = NULL
    )
    colnames(df)[1] <- "seq"
    # If the keep_suterella flag is set to false, Suterella will be removed.
    taxa <- seq_taxa(keep_suterella)
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
    df$aggression <- factor(df$aggression)
    df
}

train_ctrl <- function() {
    # 10-fold repeating three times
    caret::trainControl(
        method = "repeatedcv",
        number = 10,
        repeats = 3,
        classProbs = TRUE,
        savePredictions = TRUE # for ROC plotting
    )
}

train_anxiety <- function(df, n_tree, tune_grid) {
    caret::train(
        anxiety ~ .,
        data = df,
        method = "rf",
        tuneGrid = tune_grid,
        trControl = ctrl,
        metric = "Accuracy",
        ntree = n_tree,
        importance = TRUE
    )
}

train_aggression <- function(df, n_tree, tune_grid) {
    caret::train(
        aggression ~ .,
        data = df,
        method = "rf",
        tuneGrid = tune_grid,
        trControl = ctrl,
        metric = "Accuracy",
        ntree = n_tree,
        importance = TRUE
    )
}