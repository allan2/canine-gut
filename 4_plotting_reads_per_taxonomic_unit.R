library(ggplot2)

df <- read.table("data/SeqTab_NoChim_SamplesInColumns_Taxa.tsv",
    sep = "\t", header = TRUE
)
# Drop the sequence column.
df <- df[, !(names(df) == "Sequence")]

num_sample_cols <- sum(!
colnames(df) %in% c(
    "Kingdom",
    "Phylum",
    "Class",
    "Order",
    "Family",
    "Genus"
))

# Convert to numeric. This produces NA warnings.
df[1:num_sample_cols] <- sapply(
    df[1:num_sample_cols],
    as.numeric
)

# Found 1737 NAs
print(sum(is.na(df)))
df[is.na(df)] <- 0

sums <- rowSums(df[1:num_sample_cols])
sums <- data.frame(cbind(seq(sums), sums, df$Genus))
colnames(sums) <- c("id", "read_count", "genus")

# Bar plot reads per OTU.
ggplot(sums, aes(x = id, y = read_count)) +
    geom_bar(
        stat = "identity",
        fill = "blue"
    ) +
    xlab("OTU Index") +
    ylab("Number of Reads") +
    ggtitle("Read Count by OTU") +
    # Center the title.
    theme(plot.title = element_text(hjust = 0.5))
ggsave("1_reads_by_otu.png")


# Convert to numeric. This produces NA warnings.
sums$read_count <- sapply(
    sums$read_count,
    as.numeric
)

# Read count totals per genus group
genus_counts <- aggregate(sums$read_count,
    by = list(genus = sums$genus), FUN = sum
)
colnames(genus_counts) <- c("genus", "read_count")

# Order by read count descending.
genus_counts <- genus_counts[order(genus_counts$read_count,
    decreasing = TRUE
), ]
genus_counts_top_20 <- genus_counts[1:20, ]

# Bar plot reads per OTU.
ggplot(genus_counts_top_20, aes(x = genus, y = read_count)) +
    geom_bar(
        stat = "identity",
        fill = "blue"
    ) +
    xlab("Genus") +
    ylab("Number of Reads") +
    ggtitle("Read Count by Genus (top 20)") +
    # Center the title.
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(
        angle = 90,
        vjust = 0.5, hjust = 1
    ))
ggsave("2_reads_by_genus_top_20.png")