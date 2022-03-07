seq_taxa <- function() {
    df <- read.table("data/SeqTab_NoChim_SamplesInColumns_Taxa.tsv",
        sep = "\t", header = TRUE, row.names = NULL
    )
    colnames(df)[1] <- "seq"

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