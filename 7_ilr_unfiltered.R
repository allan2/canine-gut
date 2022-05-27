source("find_hyperparams.R")

df_all <- read.table("data/ILR.tsv",
    sep = "\t", header = TRUE, row.names = 1
)
# Transpose so we get samples as columns.
# TODO: possible improvement - see if call to tranpose in prep() can be removed
df_all <- t(df_all)

# No filtering on taxa because we do not have the sequence as index

# Anxiety, all dogs. Remove aggression column
df <- prep(df_all)
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
    "results_ilr.csv",
    append = FALSE,
    sep = ",",
    row.names = TRUE,
    col.names = NA # leave a blank in the header for the data frame index
)