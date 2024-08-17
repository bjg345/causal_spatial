#!/bin/bash

#$ -l mem_free=12G
#$ -l h_vmem=12G
#$ -l h_rt=720:00:00
#$ -cwd
#$ -j y
#$ -R y
#$ -t 1:250
module load conda_R
cd ..
Rscript sim1.R $SGE_TASK_ID 'spline3'

