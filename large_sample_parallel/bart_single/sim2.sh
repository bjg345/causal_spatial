#!/bin/bash
#$ -pe local 24
#$ -l mem_free=1.5G
#$ -l h_vmem=1.5G
#$ -l h_rt=720:00:00
#$ -cwd
#$ -j y
#$ -R y
#$ -t 1:250
cd ..
module load conda_R
Rscript sim2.R $SGE_TASK_ID 'bart_single'

