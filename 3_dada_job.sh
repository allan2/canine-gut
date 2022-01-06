#!/bin/env bash

#SBATCH --job-name=canine-gut-dada2
#SBATCH --mem=3200M
#SBATCH --time=00:15:00

module load gcc/9.3.0 r-bundle-bioconductor/3.14

readonly OUTPUT_DIR='output_dada2'
mkdir -p $OUTPUT_DIR

printf -v start_time '%(%s)T'
logfile="$OUTPUT_DIR/log_$(printf '%(%Y%m%d_%H%M%S)T' $start_time).log"

Rscript dada2.R -o $OUTPUT_DIR
printf -v end_time '%(%s)T'

# Print the elapsed time and write it to the log.
elapsed=$((end_time - start_time))
elapsed_msg="Elapsed time: $(($elapsed / 60))m $(($elapsed % 60))s"
printf %s "$elapsed_msg" | tee -a $logfile

exit 0