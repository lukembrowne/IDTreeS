#!/bin/bash
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --partition=day
#SBATCH --time=2:00:00
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=lukembrowne@gmail.com
#SBATCH --output=./logs/job_%A_%a.out



# To submit as job array
# sbatch --array=1-585  ./code/01_crop_images_cluster.sh

# module load R
# module load GDAL
#
# # Run script to process image
# Rscript --vanilla ./code/01_crop_images.R ${SLURM_ARRAY_TASK_ID}
#
# echo ${SLURM_ARRAY_TASK_ID}
# echo 'JOB DONE'
#
#

# Or create job_list for deadsimple que
# https://docs.ycrc.yale.edu/clusters-at-yale/job-scheduling/dsq/

rm ./code/01_crop_joblist_test_data.txt
touch ./code/01_crop_joblist_test_data.txt
for i in {1..585};
  do
  echo "module load R; module load GDAL; Rscript --vanilla ./code/01_crop_images_test_data.R $i" >> ./code/01_crop_joblist_test_data.txt
  done

module load dSQ

# Set these environmental variables to load sf package
UDUNITS2_LIBS=/home/lmb242/udunits/local/lib
UDUNITS2_INCLUDE=/home/lmb242/udunits/local/include
LD_LIBRARY_PATH=/home/lmb242/udunits/local/lib:$LD_LIBRARY_PATH


# Create job file for dsq
dsq --job-file ./code/01_crop_joblist_test_data.txt --mem-per-cpu 16g -t 1:00:00 -o ./logs/dsq-jobfile-%A_%a-%N.out --submit

# Find jobs that failed using jobID
dSQAutopsy -j 60719557 --job-file ./code/01_crop_joblist_test_data.txt --states FAILED > ./code/failed_jobs_test_data.txt

# Resubmit failed jobs
dsq --job-file ./code/failed_jobs_test_data.txt --mem-per-cpu 16g -t 1:00:00 -o ./logs/dsq-jobfile-%A_%a-%N.out --submit

## Count number of output files
find ./data/IDTREES_competition_test/task2/RemoteSensing_processed/HSI_by_species_cropped/  -name *.tif | wc -l
