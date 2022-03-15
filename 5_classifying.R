
source("find_hyperparams.R")

read_file <- function(filename) {
    read.table(filename,
        sep = "\t", header = TRUE, row.names = 1
    )
}

# Read count data.
df_24 <- read_file("data/SeqTab_NoChim_SamplesInColumns_in24Dogs.tsv")
df_all <- read_file("data/SeqTab_NoChim_SamplesInColumns.tsv")

read_taxa <- function() {
    df <- read_file("data/SeqTab_NoChim_SamplesInColumns_Taxa.tsv")
    df[, colnames(df) %in% c("Class", "Order", "Family", "Genus")]
}

taxa <- read_taxa()

# Anxiety for 24 dogs
family_keep <- c(
    "Bacteroidaceae", "Ruminococcaceae"
)
genus_keep <- c("Blautia", "Faecalibacterium", "Bacteroides")
# Filter taxa to keep only the families and genuses we want.
taxa_tmp <- taxa[
    (taxa$Order == "Oscillospirales" |
        taxa$Family %in% family_keep |
        taxa$Genus %in% genus_keep),
]
df <- filter_taxa(df_24, taxa_tmp)
df <- prep(df)
# Remove the aggression column.
df_anx_24 <- subset(df, select = -c(aggression))

# Aggression for 24 dogs
taxa_tmp <- taxa[
    (
        taxa$Class == "Clostridia" |
            taxa$Order == "Oscillospirales" |
            taxa$Family == "Ruminococcaceae" |
            taxa$Genus == "Faecalibacterium"
    ),
]
df <- filter_taxa(df_24, taxa_tmp)
df <- prep(df)
# Remove the anxiety column.
df_aggr_24 <- subset(df, select = -c(anxiety))


# Anxiety, all dogs

genus_keep <- c("Blautia", "Faecalibacterium")
order_keep <- c("Clostridia", "Erysipelotrichales", "Oscillospirales")
taxa_tmp <- taxa[
    (taxa$Order %in% order_keep |
        taxa$Class == "Bacilli" |
        taxa$Genus %in% genus_keep),
]
df <- filter_taxa(df_all, taxa_tmp)
df <- prep(df)
df_anx_all <- subset(df, select = -c(aggression))

# Aggression, all dogs
order_keep <- c("Acidaminococcales", "Oscillospirales")
family_keep <- c(
    "Succinivibrionaceae", "Acidaminococcaceae", "Ruminococcaceae"
)
genus_keep <- c("Phascolarctobacterium", "Faecalibacterium")
taxa_tmp <- taxa[
    (taxa$Order %in% order_keep | taxa$Family %in% family_keep |
        taxa$Genus %in% genus_keep),
]
df <- filter_taxa(df_all, taxa_tmp)
df <- prep(df)
df_aggr_all <- subset(df, select = -c(anxiety))


ctrl <- train_ctrl()
results <- data.frame()
set.seed(1)
# Only keep our six genuses/families.
anx_24 <- run("anx_24", df_anx_24, ctrl, train_anxiety)
aggr_24 <- run("aggr_24", df_aggr_24, ctrl, train_aggression)
anx_all <- run("anx_full", df_anx_all, ctrl, train_anxiety)
aggr_all <- run("aggr_full", df_aggr_all, ctrl, train_aggression)

results <- rbind(anx_24, aggr_24, anx_all, aggr_all)
write.table(results,
    "results.csv",
    append = FALSE,
    sep = ",",
    row.names = TRUE,
    col.names = NA # leave a blank in the header for the data frame index
)