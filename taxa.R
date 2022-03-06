seq_taxa <- function(keep_suterella) {
    df <- read.table("data/SeqTab_NoChim_SamplesInColumns_Taxa.tsv",
        sep = "\t", header = TRUE, row.names = NULL
    )
    colnames(df)[1] <- "seq"

    if (!keep_suterella) {
        # Filter out Sutterella due to anomalies.
        df <- df[!(df$Genus == "Sutterella"), ]
    }

    # Define taxanomic columns.
    tx_cols <- c(
        "seq",
        "Kingdom",
        "Phylum",
        "Class",
        "Order",
        "Family",
        "Genus"
    )
    # Only keep taxa columns.
    df[, colnames(df) %in% tx_cols]
}