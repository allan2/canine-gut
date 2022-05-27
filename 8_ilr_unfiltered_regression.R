source("find_hyperparams.R")

df_all <- read.table("data/ILR.tsv",
    sep = "\t", header = TRUE, row.names = 1
)
# Transpose so we get samples as columns.
# TODO: possible improvement - see if call to tranpose in prep() can be removed
df_all <- t(df_all)

# No filtering on taxa because we do not have the sequence as index

# Instead of prep() from finding_hyperparams.R, we are using a new one to use continuous values for aggression and anxiety
prep_cont <- function(df) {
    # Transpose so that OTUs are columns. The index is "SampleN".
    df <- as.data.frame(t(df))
    # We don't need the full sequence as the column names.
    # Replace them with numbers.
    colnames(df) <- seq_len(length(colnames(df)))

    # The index is "SampleN". It has been renamed.
    sample_vars <- read.csv("data/cbarq_continuous_renamed_idx.csv",
        header = TRUE, row.names = 1
    )

    if (nrow(df) != nrow(sample_vars)) {
        stop("Number of samples found does not match")
    }

    # Only keep the two classifiers.
    colnames(sample_vars) <- tolower(colnames(sample_vars))
    sample_vars <- sample_vars[, names(sample_vars) %in% c(
        "anxiety", "aggression"
    )]

    # Inner join.
    # We now have a data frame with sample, read_count, anxiety, and aggression.
    df <- merge(x = df, y = sample_vars, by = 0)
    # A "Row.names" column was created. Remove it.
    df <- subset(df, select = -c(Row.names))
    df
}

# TODO: add metric as a function param into find_hyperparams
train_anxiety <- function(df, ctrl, n_tree, tune_grid) {
    caret::train(
        anxiety ~ .,
        data = df,
        method = "rf",
        tuneGrid = tune_grid,
        trControl = ctrl,
        metric = "RMSE",
        ntree = n_tree,
        importance = TRUE
    )
}

train_aggression <- function(df, ctrl, n_tree, tune_grid) {
    caret::train(
        aggression ~ .,
        data = df,
        method = "rf",
        tuneGrid = tune_grid,
        trControl = ctrl,
        metric = "RMSE",
        ntree = n_tree,
        importance = TRUE
    )
}


df <- prep_cont(df_all)
df_anx_all <- subset(df, select = -c(aggression))
# Aggression, all dogs. Remove anxiety column
df_aggr_all <- subset(df, select = -c(anxiety))

ctrl <- train_ctrl()
results <- data.frame()
set.seed(1)
# Only keep our six genuses/families.
anx_all <- run("anx_full", df_anx_all, ctrl, train_anxiety)
aggr_all <- run("aggr_full", df_aggr_all, ctrl, train_aggression)

results <- rbind(anx_all, aggr_all)
write.table(results,
    "results_ilr_regression.csv",
    append = FALSE,
    sep = ",",
    row.names = TRUE,
    col.names = NA # leave a blank in the header for the data frame index
)