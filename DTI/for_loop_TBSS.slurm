#!/bin/bash
#SBATCH -J TBSS
#SBATCH -a 1-279
#SBATCH --time=20:00:00
#SBATCH --mem=300000
#SBATCH --cpus-per-task=4
#SBATCH -o output/out_TBSS.%A_%a
#SBATCH -e output/err_TBSS.%A_%a

home=/DWIdata2023/yasmin
caselist=//DWIdata2023/yasmin/mytbss/caselist.txt
case=`head -${SLURM_ARRAY_TASK_ID} ${caselist} | tail -1`

# the following code prints the date
start=`date +%s`
echo ${start}

# the following two lines selects the case from caselist and stores it into case

echo ${SLURM_ARRAY_TASK_ID}

module load FSL/6.0.6.4

cd ${home}/mytbss

tbss_1_preproc sub-${case}_FA.nii.gz

