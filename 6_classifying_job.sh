#!/bin/env bash

#SBATCH --job-name=canine-gut-rf-classification
#SBATCH --mem=32000M
#SBATCH --time=48:00:00
#SBATCH --mail-user=azhang@mun.ca
#SBATCH --mail-type=ALL

module load gcc/9.3.0 r-bundle-bioconductor/3.14

readonly OUTPUT_DIR='find-hyperparams'
mkdir -p $OUTPUT_DIR

printf -v start_time '%(%s)T'
logfile="$OUTPUT_DIR/log_$(printf '%(%Y%m%d_%H%M%S)T' $start_time).log"

Rscript 5_classifying.R -o $OUTPUT_DIR
printf -v end_time '%(%s)T'

# Print the elapsed time and write it to the log.
elapsed=$((end_time - start_time))
elapsed_msg="Elapsed time: $(($elapsed / 60))m $(($elapsed % 60))s"
printf %s "$elapsed_msg" | tee -a $logfile

exit 0
