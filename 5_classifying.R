source("find_hyperparams.R")
source("taxa.R")


output_filename <- function(label, idx, keep_suterella) {
    filename <- paste0("output_", label)
    if (!keep_suterella) {
        filename <- paste0(filename, "_nosut")
    }
    paste0(filename, idx, ".csv")
}

# The sample dataset
run_rf <- function(keep_suterella) {
    df <- prep(keep_suterella)
    # Remove the aggression column from the anxiety table.
    df_anx <- subset(df, select = -c(aggression))
    # Remove the anxiety column from the aggression table.
    df_aggr <- subset(df, select = -c(anxiety))

    set.seed(1)
    n_trees <- seq(100, 500, 100)
    # Because we remove either accuracy or aggression column
    n_col <- ncol(df) - 1
    m_values <- c(log(n_col), sqrt(n_col), (n_col / 4))
    ctrl <- train_ctrl()

    # Anxiety
    models <- list()
    idx <- 0
    keep_sut <- c(TRUE, FALSE)
    print("Running anxiety training")
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
            write.table(res,
                output_filename("anx_full", idx, keep_suterella),
                header = FALSE,
                append = FALSE,
                sep = ",",
                row.names = TRUE,
                col.names = TRUE
            )
        }
    }

    # Aggression
    models <- list()
    idx <- 0
    print("Running aggression training")
    for (m in m_values) {
        idx <- idx + 1
        # Use the entire data set because we have few dogs.
        tune_grid <- expand.grid(.mtry = m)
        for (n_tree in n_trees) {
            start_time <- Sys.time()
            fit <- train_aggression(df_aggr, n_tree, tune_grid)
            end_time <- Sys.time()
            elapsed <- end_time - start_time
            print(elapsed)
            models[[toString(n_tree)]] <- fit
            res <- data.frame(fit$results)
            write.table(res,
                output_filename("anx_full", idx, keep_suterella),
                header = FALSE,
                append = FALSE,
                sep = ",",
                row.names = TRUE,
                col.names = TRUE
            )
        }
    }
}

# Run
run_rf(keep_suterella = TRUE)
run_rf(keep_suterella = FALSE)