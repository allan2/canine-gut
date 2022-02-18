library(caret)
library(mlbench)
library(tidyverse)
library(ggplot2)
library(plotROC)

sample_mode <- FALSE

# Define taxanomic columns. They will be excluded.
tx_cols <- c(
    "Kingdom",
    "Phylum",
    "Class",
    "Order",
    "Family",
    "Genus"
)
# Read count data
df <- read.table("data/SeqTab_NoChim_SamplesInColumns_in24Dogs.tsv",
    sep = "\t", header = TRUE
)
# Drop the sequence column.
df <- df[, !(names(df) == "Sequence")]
# Drop the taxonomic columns.
df <- df[, !colnames(df) %in% tx_cols]
# Reshape columns into rows with a supporting "sample_num" column.
df <- df %>% tidyr::gather(
    sample, read_count
)

sample_vars <- read.csv("data/samplevariables.csv",
    header = TRUE, row.names = NULL
)
# Rename the first column to match our read count data frame.
names(sample_vars)[1] <- "sample"
colnames(sample_vars) <- tolower(colnames(sample_vars))
# Only keep the two classifiers and the sample number.
sample_vars <- sample_vars[, names(sample_vars) %in% c(
    "sample",
    "anxiety", "aggression"
)]

# Left join.
# We now have a data frame with sample, read_count, anxiety, and aggression.
df <- merge(x = df, y = sample_vars, by = "sample", all.x = TRUE)

# Convert classifiers to factors.
df$anxiety <- factor(df$anxiety)
df$aggression <- factor(df$aggression)

# caret::createDataPartition asks for one outcome vector.
# Feed in the concatenated outcome for the random split.
df$outcome <- paste0(df$anxiety, df$aggression)
df$outcome <- factor(df$outcome)

if (sample_mode) {
    df <- df[sample(nrow(df), 100), ]
}

# Stratied random split of the data
in_train <- caret::createDataPartition(
    y = c(df$outcome),
    p = .75,
    list = FALSE
)
training <- df[in_train, ]
testing <- df[-in_train, ]
if ((nrow(testing) + nrow(training)) != nrow(df)) {
    warning("Something went wrong with data partioning")
}

# 10-fold repeating three times
ctrl <- caret::trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 3,
    classProbs = TRUE,
    savePredictions = TRUE # for ROC plotting
)
m <- sqrt(ncol(training))
tune_grid <- expand.grid(.mtry = c(sqrt(ncol(training))))
set.seed(1251)

# Tune manually by varying the number of trees.
n_trees <- seq(100, 500, 100)
models <- list()
# Train.
for (n_tree in n_trees) {
    start_time <- Sys.time()
    fit <- caret::train(
        outcome ~ .,
        data = training,
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