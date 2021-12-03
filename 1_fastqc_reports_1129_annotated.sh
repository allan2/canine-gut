#!/bin/bash

# This is an annotated version for someone who is new to shell scripts.
#
# The first line is called a shebang. It tells us which interpreter to use.
# There are multiple shell interpreters, incluing Bash and Zsh. Bash is the most common one.
#
# These SBATCH lines are interpreted by Compute Canada for the job scheduler.

#SBATCH --job-name=canine-gut-fastqc
#SBATCH --mem=3200M
#SBATCH --time=00:15:00

module load fastqc/0.11.9

# We define the output directory. It's easier to see where things go and to change variables when we put it at the top.
readonly OUTPUT_DIR='output_fastqc'

# This makes a directory. The -p flag ensures that the directory will be created if it doesn not exist.
# If it does exist, there won't be an error.
mkdir -p $OUTPUT_DIR

# We get the start time in epoch seconds. The printf command is a built-in Bash function that is more performant than calling echo, which would launch a subshell.
printf -v start_time '%(%s)T'
logfile="$OUTPUT_DIR/log_$(printf '%(%Y%m%d_%H%M%S)T' $start_time).log"
# The pipe allows the output of the command to be piped to the logfile.
fastqc *.fastq.gz -o $OUTPUT_DIR | tee $logfile
printf -v end_time '%(%s)T'

# Print the elapsed time and write it to the log.
elapsed=$((end_time - start_time))
elapsed_msg="Elapsed time: $(($elapsed / 60))m $(($elapsed % 60))s"
printf %s "$elapsed_msg" | tee -a $logfile

# Exit code 0 indicates termination without errors. If we got to this point, then there must have been no errors!
# Hence, we signal the outcome of the script.
exit 0
