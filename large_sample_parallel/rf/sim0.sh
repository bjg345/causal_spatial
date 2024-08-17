#!/bin/bash
#$ -pe local 6
#$ -l mem_free=2G
#$ -l h_vmem=2G
#$ -l h_rt=720:00:00
#$ -cwd
#$ -j y
#$ -R y
#$ -t 1:250
module load conda_R
cd ..
Rscript sim0.R $SGE_TASK_ID 'rf_single'

