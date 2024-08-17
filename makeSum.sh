#!/bin/bash

#$ -l mem_free=1.2G
#$ -l h_vmem=1.2G
#$ -l h_rt=720:00:00
#$ -cwd
#$ -j y
#$ -R y

module load conda_R
Rscript makeSum.R

