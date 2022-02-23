library(caret)
library(mlbench)
library(tidyverse)
library(ggplot2)
library(plotROC)

sample_mode <- TRUE

# Read count data.
df <- read.table("data/SeqTab_NoChim_SamplesInColumns_in24Dogs.tsv",
    sep = "\t", header = TRUE, row.names = 1
)
# Transpose so that OTUs are columns. The index is "SampleN".
df <- as.data.frame(t(df))
# We don't need the full sequence as the column names. Let's replace them with numbers.
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
    df <- df[sample(nrow(df), 20), ]
}

set.seed(1251)

# Train a random forest for a given classifier.
train_anxiety <- function(n_tree, tune_grid) {
    # 10-fold repeating three times
    ctrl <- caret::trainControl(
        method = "repeatedcv",
        number = 10,
        repeats = 3,
        classProbs = TRUE,
        savePredictions = TRUE # for ROC plotting
    )

    # Train.
    start_time <- Sys.time()
    fit <- caret::train(
        anxiety ~ .,
        data = df,
        method = "rf",
        tuneGrid = tune_grid,
        trControl = ctrl,
        metric = "Accuracy",
        ntree = n_tree
    )
    end_time <- Sys.time()
    elapsed <- end_time - start_time
    print(elapsed)
    fit
}


n_trees <- seq(100, 500, 100)
m_values <- c(log(ncol(df)), sqrt(ncol(df)), ncol(df) / 4)

# Store results
models <- list()
for (m in m_values) {
    # Use the entire data set because we have few dogs.
    tune_grid <- expand.grid(.mtry = c(sqrt(ncol(df))))
    for (n_tree in n_trees) {
        fit <- train_anxiety(n_tree, tune_grid)
        models[[toString(n_tree)]] <- fit
    }
}
results <- caret::resamples(models)
summary(results)
png("output/5_dotplot_accuracy_anxiety.png")
lattice::dotplot(results)
dev.off()
# TODO: Only labels with 2 classes supported
ggplot(fit$pred, aes(
    m = fit$pred$M,
    d = fit$pred$obs
)) +
    geom_roc(hjust = -0.4, vjust = 1.5) +
    coord_equal()