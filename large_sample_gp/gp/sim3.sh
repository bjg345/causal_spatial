#!/bin/bash
#$ -pe local 6
#$ -l mem_free=5G
#$ -l h_vmem=5G
#$ -l h_rt=720:00:00
#$ -cwd
#$ -j y
#$ -R y
#$ -t 1:250
cd ..
module load conda_R
Rscript sim3.R $SGE_TASK_ID 'gp'

