#!/bin/bash
#SBATCH -J TBSS
#SBATCH -a 1
#SBATCH --time=1-00:00:00
#SBATCH --mem=300000
#SBATCH --cpus-per-task=10

#SBATCH -o output/out_TBSS.%A_%a
#SBATCH -e output/err_TBSS.%A_%a

home=/data/gpfs/projects/punim1958/DWIdata2023/yasmin/mytbss/stats
caselist=/data/gpfs/projects/punim1958/DWIdata2023/yasmin/mytbss/caselist.txt


# the following code prints the date
start=`date +%s`
echo ${start}

# the following two lines selects the case from caselist and stores it into case

echo ${SLURM_ARRAY_TASK_ID}

module load FSL/6.0.6.4

cd ${home}

randomise -i all_FA_skeletonised -o tbss -m mean_FA_skeleton_mask -d design.mat -t design.con -n 5000 --T2
