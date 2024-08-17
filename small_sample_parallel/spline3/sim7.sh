#!/bin/bash
#$ -pe local 24
#$ -l mem_free=2G
#$ -l h_vmem=2G
#$ -l h_rt=720:00:00
#$ -cwd
#$ -j y
#$ -R y
#$ -t 1:500
cd ..
module load conda_R
Rscript sim7.R $SGE_TASK_ID 'spline3'

