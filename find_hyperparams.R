library(caret)

# Reads the data into the form we want.
# samples_df - data frame of samples with sequences as rows
#              and samples as columns
# taxa - data frame with taxanomical data.
filter_taxa <- function(df, taxa) {
    # Inner join on the sequence ID (the index).
    df <- merge(df, taxa, by = 0)
    # Remove taxa columns and the "Row.name" column that was created after merge.
    subset(df, select = -c(Row.names, Class, Order, Family, Genus))
}


# Reads the data into the form we want.

prep <- function(df) {
    # Transpose so that OTUs are columns. The index is "SampleN".
    df <- as.data.frame(t(df))
    # We don't need the full sequence as the column names.
    # Replace them with numbers.
    colnames(df) <- seq_len(length(colnames(df)))

    # The index is "SampleN".
    sample_vars <- read.csv("data/samplevariables.csv",
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



# The function to run the training through different values of m and n_tree
# Output is saved as a csv.
#
# job_name: e.g., anx_24, aggr_full
# df: data frame with the samples as rows,
#     the sequences as columns, and the classifier as a column
# ctrl: output of train_ctrl()
# train_fn: the training function, such as train_anxiety or train_aggression
run <- function(job_name, df, ctrl, train_fn) {
    print(paste0("Running ", job_name, "..."))
    n_trees <- seq(100, 500, 100)
    # Because we remove either accuracy or aggression column
    n_col <- ncol(df) - 1
    m_values <- c(log(n_col), sqrt(n_col), (n_col / 4))
    ctrl <- train_ctrl()
    idx <- 0
    results <- data.frame()
    for (m in m_values) {
        # Use the entire data set because we have few dogs.
        tune_grid <- expand.grid(.mtry = m)
        for (n_tree in n_trees) {
            idx <- idx + 1
            start_time <- Sys.time()
            fit <- train_fn(df, ctrl, n_tree, tune_grid)
            end_time <- Sys.time()
            elapsed <- end_time - start_time
            print(paste0(
                "Iteration ", idx, "    m: ", round(m, 1),
                "    n_tree: ", n_tree, "    Elapsed: ", round(elapsed, 1), " s"
            ))
            res <- data.frame(fit$results)
            results <- rbind(results, res)
        }
    }
    cbind(job_name = job_name, results)
}