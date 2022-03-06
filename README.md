# canine-gut

To run the FastQC Reports script:

```bash
cd canine_gut_data

# Reports
chmod +x 1_fastqc_reports.sh
./1_fastqc_reports.sh

# Trimming
chmod +x 2_trim.sh
./2_trim.sh *.fastq.gz
```

# Data Folder

- `out_dada2_filterAndTrim.csv` - a table with sequence FastQ filename, reads in, and reads out
