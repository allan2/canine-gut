library(ggplot2)
library(tidyverse)

df <- read.table("data/SeqTab_NoChim_SamplesInColumns_Taxa.tsv",
    sep = "\t", header = TRUE
)
# Drop the sequence column.
df <- df[, !(names(df) == "Sequence")]

# Filter out Sutterella due to anomalies.
df <- df[!(df$Genus == "Sutterella"), ]

# Define taxanomic columns.
tx_cols <- c(
    "Kingdom",
    "Phylum",
    "Class",
    "Order",
    "Family",
    "Genus"
)

num_sample_cols <- sum(!
colnames(df) %in% tx_cols)

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

median_sums <- median(sums$read_count)

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
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
    ) +
    geom_hline(
        yintercept = median_sums,
        color = "red"
    )
ggsave("output/1_reads_by_otu.png")

sums <- sums[order(sums$read_count,
    decreasing = TRUE
), ]

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
if (sum(genus_counts_top_20$genus == "0")) {
    genus_counts_top_20[
        which(genus_counts_top_20$genus == "0"),
    ][1] <- "Unknown"
}

# manual fix of names
genus_counts_top_20[
    which(genus_counts_top_20$genus == "[Ruminococcus]"),
][1] <- "Rumminococcus"

genus_counts_top_20$read_count_log <- log2(genus_counts_top_20$read_count)

# Bar plot reads per OTU.
ggplot(genus_counts_top_20, aes(
    x = reorder(genus, -read_count_log),
    y = read_count_log
)) +
    geom_bar(
        stat = "identity",
        fill = "blue"
    ) +
    xlab("Genus") +
    ylab("Log2(Number of Reads)") +
    ggtitle("Log2 of Total Read Count by Genus (top 20)") +
    # Center the title.
    theme(
        plot.title = element_text(hjust = 0.5),
        # Rotate the x-axis labels.
        axis.text.x = element_text(
            angle = 90,
            vjust = 0.5, hjust = 1
        )
    )
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

# Colour-blind friendly
cb_palette <- c(
    "#999999",
    "#e69f00",
    "#56b4e9",
    "#009e73",
    "#f0E442",
    "#0072b2",
    "#d55e00",
    "#cc79a7"
)

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
    scale_fill_manual(values = cb_palette) +
    theme_void() +
    theme(
        plot.title = element_text(hjust = 0.5)
    )
ggsave("output/3_pie_chart_reads_by_genus_top_7.png")

# Inner join with the top 20 genuses.
sums_top_20 <- merge(x = sums, y = genus_counts_top_20, by = "genus")
sums_top_20$read_count_log <- log2(sums_top_20$read_count.x)

# Box plots by genus
ggplot(sums_top_20, aes(
    x = reorder(genus, -read_count_log),
    y = read_count_log
)) +
    geom_boxplot(
        outlier.colour = "black",
        outlier.shape = 16,
        outlier.size = 2,
        notch = FALSE
    ) +
    xlab("Genus") +
    ylab("Log2(Number of Reads)") +
    ggtitle("Log2(Read Count) by Genus") +
    # Center the title.
    theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(
            angle = 90,
            vjust = 0.5, hjust = 1
        )
    )
ggsave("output/2_genus_top_20_boxplot.png")

# We merge to get the group by totals to sort on for the box plot.
sums_all <- merge(x = sums, y = genus_counts, by = "genus")
sums_all$read_count_log <- log2(sums_all$read_count.x)

ggplot(sums_all, aes(x = reorder(genus, -read_count.y), y = read_count_log)) +
    geom_boxplot(
        outlier.colour = "black",
        outlier.shape = 16,
        outlier.size = 2,
        notch = FALSE
    ) +
    xlab("Genus") +
    ylab("Number of Reads") +
    ggtitle("Read Count by Genus") +
    # Center the title.
    theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
    )
ggsave("output/2_genus_all_boxplot.png")

# box plot by sample

# We want a column with sample number and the read count.
# Turn the sample attributes into a single attribute, "sample number".

# Only keep sample columns.
sample_df <- df[, !colnames(df) %in% tx_cols]
nrow(data.frame(x = unlist(sample_df)))
colnames(sample_df) <- gsub("Sample", "", colnames(sample_df))
reshaped <- sample_df %>% tidyr::gather(
    sample_num, read_count, seq_len(ncol(sample_df))
)
if (!(sum(reshaped$read_count) == sum(sample_df))) {
    stop("Something went wrong reshaping")
}

reshaped$sample_num <- as.numeric(reshaped$sample_num)

ggplot(reshaped, aes(
    x = sample_num,
    y = read_count, group = cut_interval(sample_num, length = 1)
)) +
    geom_boxplot(
        outlier.colour = "black",
        notch = FALSE
    ) +
    xlab("Sample Number") +
    ylab("Number of Reads") +
    ggtitle("Read Count by Sample Number") +
    # Center the title.
    theme(
        plot.title = element_text(hjust = 0.5)
    )
ggsave("output/2_boxplot_sample_number.png")