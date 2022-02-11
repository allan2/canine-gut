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
sums$read_count <- as.numeric(sums$read_count)

# Bar plot reads per OTU.
ggplot(sums, aes(x = reorder(id, -read_count), y = read_count)) +
    geom_bar(
        stat = "identity",
        fill = "blue"
    ) +
    xlab("OTU") +
    ylab("Number of Reads") +
    ggtitle("Read Count by OTU") +
    # Center the title.
    theme(
        plot.title = element_text(hjust = 0.5),
        # Hide the x axis labels, i.e., OTU ID.
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
    )
ggsave("output/1_reads_by_otu.png")

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

# Rename the missing genus.
genus_counts_top_20[
    which(genus_counts_top_20$genus == "0"),
][1] <- "Unknown"

# manual fix of names
genus_counts_top_20[
    which(genus_counts_top_20$genus == "[Ruminococcus]"),
][1] <- "Rumminococcus"

# Bar plot reads per OTU.
ggplot(genus_counts_top_20, aes(
    x = reorder(genus, -read_count),
    y = read_count
)) +
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
    )) +
    # Don't show scientific notation.
    scale_y_continuous(labels = scales::comma)
ggsave("output/2_reads_by_genus_top_20.png")

# Pie chart

other <- sum(genus_counts[(7:nrow(genus_counts)), ]$read_count)
# Get top six and the other group.
genus_counts_top_6 <- genus_counts[(1:6), ]
other_row <- data.frame(
    genus = "Other",
    read_count = other
)
genus_counts_top_7 <- rbind(genus_counts_top_6, other_row)
if (!(sum(genus_counts$read_count) == sum(genus_counts_top_7$read_count))) {
    stop("Pie chart total is incorrect")
}

ggplot(genus_counts_top_7, aes(
    x = "",
    y = read_count,
    fill = genus
)) +
    geom_bar(
        stat = "identity",
    ) +
    coord_polar("y", start = 0) +
    ggtitle("Read Count by Genus") +
    theme_void()
ggsave("output/3_pie_chart_reads_by_genus_top_7.png")