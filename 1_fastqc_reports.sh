#!/bin/bash

#SBATCH --job-name=canine-gut-fastqc
#SBATCH --mem=3200M
#SBATCH --time=00:15:00

module load fastqc/0.11.9

readonly OUTPUT_DIR="output_fastqc"
mkdir -p $OUTPUT_DIR

printf -v start_time '%(%s)T'
sleep 2
printf 'Hello\n'
fastqc *.fastq.gz -o ${OUTPUT_DIR} | tee ${OUTPUT_DIR}/log_$(date -d @${start_time} +%Y%m%d_%H%M%S).log
printf -v end_time '%(%s)T'

elapsed=$((end_time - start_time))
elapsed_txt="Elapsed time: (($elapsed / 60))m $(($elapsed % 60))s"
echo $elapsed_txt
printf "%s" $elapsed_txt
exit 0
