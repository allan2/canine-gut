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
    taxa <- seq_taxa()
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

train_anxiety <- function(df, ctrl, n_tree, tune_grid) {
    print(nrow(df))
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

train_aggression <- function(df, ctrl, n_tree, tune_grid) {
    print(nrow(df))
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

output_filename <- function(label, idx) {
    paste0(label, idx, ".csv")
}


run <- function(label, df, ctrl, train_fn) {
    n_trees <- seq(100, 500, 100)
    # Because we remove either accuracy or aggression column
    n_col <- ncol(df) - 1
    m_values <- c(log(n_col), sqrt(n_col), (n_col / 4))
    ctrl <- train_ctrl()
    idx <- 0
    for (m in m_values) {
        print(idx)
        idx <- idx + 1
        # Use the entire data set because we have few dogs.
        tune_grid <- expand.grid(.mtry = m)
        for (n_tree in n_trees) {
            start_time <- Sys.time()
            fit <- train_fn(df, ctrl, n_tree, tune_grid)
            end_time <- Sys.time()
            elapsed <- end_time - start_time
            print(elapsed)
            res <- data.frame(fit$results)
            filename <- paste0(label, idx, ".csv")
            write.table(res,
                filename,
                append = FALSE,
                sep = ",",
                row.names = FALSE,
                col.names = TRUE
            )
        }
    }
}