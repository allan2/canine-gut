#!/bin/bash

#SBATCH --job-name=canine-gut-fastqc
#SBATCH --mem=3200M
#SBATCH --time=00:15:00

readonly OUTPUT_DIR='output_fastqc'
mkdir -p $OUTPUT_DIR

printf -v start_time '%(%s)T'
logfile="$OUTPUT_DIR/log_$(printf '%(%Y%m%d_%H%M%S)T' $start_time).log"
fastqc *.fastq.gz -o $OUTPUT_DIR | tee $logfile
printf -v end_time '%(%s)T'

# Print the elapsed time and write it to the log.
elapsed=$((end_time - start_time))
elapsed_msg="Elapsed time: $(($elapsed / 60))m $(($elapsed % 60))s"
printf %s "$elapsed_msg" | tee -a $logfile

exit 0
