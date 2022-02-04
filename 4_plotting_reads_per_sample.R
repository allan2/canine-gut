library(ggplot2)

df <- read.csv("data/out_dada2_filterAndTrim.csv", header = TRUE)
colnames(df) <- c("id", "reads_in", "reads_out")
# We just want the number.
df$id <- sub("Sample", "", df$id)
df$id <- gsub("_.*", "", df$id)
df$id <- as.numeric(df$id)

# Plot reads per sample.
ggplot(df, aes(x = id)) +
    geom_bar(aes(y = reads_in),
        stat = "identity",
        alpha = .7, fill = "lightblue"
    ) +
    geom_bar(aes(y = reads_out),
        stat = "identity",
        alpha = .8, fill = "pink"
    ) +
    xlab("Sample Number") +
    ylab("Number of Reads") +
    ggtitle("Read Count Before and After Trimming") +
    # Center the title.
    theme(plot.title = element_text(hjust = 0.5))