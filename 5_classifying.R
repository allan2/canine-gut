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
    df <- df[sample(nrow(df), 100), ]
}

set.seed(1251)
# Use the entire data set because we have few dogs.
m <- sqrt(ncol(df))
tune_grid <- expand.grid(.mtry = c(sqrt(ncol(df))))

# Train a random forest for a given classifier (colname).
train <- function(colname) {
    # 10-fold repeating three times
    ctrl <- caret::trainControl(
        method = "repeatedcv",
        number = 10,
        repeats = 3,
        classProbs = TRUE,
        savePredictions = TRUE # for ROC plotting
    )

    # Tune manually by varying the number of trees.
    n_trees <- seq(100, 500, 100)
    models <- list()
    # Train.
    for (n_tree in n_trees) {
        start_time <- Sys.time()
        fit <- caret::train(
            colname ~ .,
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
        models[[toString(n_tree)]] <- fit
    }

    results <- caret::resamples(models)
    summary(results)
    png("output/5_dotplot_accuracy.png")
    lattice::dotplot(results)
    dev.off()

    # TODO: Only labels with 2 classes supported
    ggplot(fit$pred, aes(
        m = fit$pred$M,
        d = fit$pred$obs
    )) +
        geom_roc(hjust = -0.4, vjust = 1.5) +
        coord_equal()
}

train("anxiety")