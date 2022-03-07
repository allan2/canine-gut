source("find_hyperparams.R")

# The sample dataset
df <- prep()
# df <- df[sample(nrow(df), 200), ]
# Remove the aggression column from the anxiety table.
df_anx <- subset(df, select = -c(aggression))
# Remove the anxiety column from the aggression table.
df_aggr <- subset(df, select = -c(anxiety))
ctrl <- train_ctrl()

set.seed(1)
run("anx", df_anx, ctrl, train_anxiety)
# run("aggr", df_aggr, ctrl, train_aggression)