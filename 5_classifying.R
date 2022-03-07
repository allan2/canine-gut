source("find_hyperparams.R")

df <- prep()

# Remove the aggression column from the anxiety table.
df_anx <- subset(df, select = -c(aggression))
# Remove the anxiety column from the aggression table.
df_aggr <- subset(df, select = -c(anxiety))

ctrl <- train_ctrl()

set.seed(1)
run("anx", df_anx, ctrl, train_anxiety)
run("aggr", df_aggr, ctrl, train_aggression)

# Only keep our six genuses/families.
df_fam <- prep(select_families = TRUE)
df_anx_fam <- subset(df_fam, select = -c(aggression))
df_aggr_fam <- subset(df_fam, select = -c(anxiety))
run("anx_fam", df_anx_fam, ctrl, train_anxiety)
run("aggr_fam", df_aggr_fam, ctrl, train_aggression)