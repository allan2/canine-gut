library(caret)
library(mlbench)
library(tidyverse)
library(ggplot2)
library(plotROC)

sample_mode <- FALSE

# Read count data.
df <- read.table("data/SeqTab_NoChim_SamplesInColumns_in24Dogs.tsv",
    sep = "\t", header = TRUE, row.names = 1
)
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

if (sample_mode) {
    df <- df[sample(nrow(df), 200), ]
}

# 10-fold repeating three times
ctrl <- caret::trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 3,
    classProbs = TRUE,
    savePredictions = TRUE # for ROC plotting
)

set.seed(1)

df_anx <- subset(df, select = -c(aggression))
df_aggr <- subset(df, select = -c(anxiety))


train_anxiety <- function(n_tree, tune_grid) {
    caret::train(
        anxiety ~ .,
        data = df_anx,
        method = "rf",
        tuneGrid = tune_grid,
        trControl = ctrl,
        metric = "Accuracy",
        ntree = n_tree,
        importance = TRUE
    )
}

train_aggression <- function(n_tree, tune_grid) {
    caret::train(
        aggression ~ .,
        data = df_aggr,
        method = "rf",
        tuneGrid = tune_grid,
        trControl = ctrl,
        metric = "Accuracy",
        ntree = n_tree,
        importance = TRUE
    )
}

n_trees <- seq(100, 500, 100)
# Because we remove either accuracy or aggression column
n_col <- ncol(df) - 1
m_values <- c(log(n_col), sqrt(n_col), (n_col / 4))

if (sample_mode) {
    m_values <- m_values[1]
    n_trees <- n_trees[1]
}

# Anxiety
models <- list()
m_idx <- 0
for (m in m_values) {
    m_idx <- m_idx + 1
    # Use the entire data set because we have few dogs.
    tune_grid <- expand.grid(.mtry = m)
    for (n_tree in n_trees) {
        start_time <- Sys.time()
        fit <- train_anxiety(n_tree, tune_grid)
        end_time <- Sys.time()
        elapsed <- end_time - start_time
        print(elapsed)
        models[[toString(n_tree)]] <- fit
        res <- data.frame(fit$results)
        write.table(res, paste0(
            "output_anx", "m", m_idx, "ntree",
            n_tree, ".csv"
        ),
        append = FALSE, sep = ",", row.names = TRUE, col.names = TRUE
        )
    }
}

# Aggression
models <- list()
m_idx <- 0
for (m in m_values) {
    m_idx <- m_idx + 1
    # Use the entire data set because we have few dogs.
    tune_grid <- expand.grid(.mtry = m)
    for (n_tree in n_trees) {
        start_time <- Sys.time()
        fit <- train_aggression(n_tree, tune_grid)
        end_time <- Sys.time()
        elapsed <- end_time - start_time
        print(elapsed)
        models[[toString(n_tree)]] <- fit
        res <- data.frame(fit$results)
        write.table(res, paste0(
            "output_aggr", "m", m_idx, "ntree",
            n_tree, ".csv"
        ),
        append = FALSE, sep = ",", row.names = TRUE, col.names = TRUE
        )
    }
}