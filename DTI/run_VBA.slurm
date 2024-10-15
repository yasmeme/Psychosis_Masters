#!/bin/bash
#SBATCH -J FA
#SBATCH -a 1
#SBATCH --time=4-00:00:00
#SBATCH -o output/out_FA.%A_%a
#SBATCH -e output/err_FA.%A_%a
#SBATCH --mem=400000
#SBATCH --cpus-per-task=15

# the following code prints the date
start=`date +%s`
echo ${start}

# the following two lines selects the case from caselist and stores it into case

echo ${SLURM_ARRAY_TASK_ID}

home=/DWIdata2023/harmonized_data/included
yasmin=/DWIdata2023/template/2024_stats

#modules to load
module load MRtrix/3.0.4

mrclusterstats ${home}/input_files_nicotine_FA.txt ${yasmin}/nicotine/design_matrix_FD.txt ${yasmin}/nicotine/contrast_matrix_FD.txt ${yasmin}/padded_WM_mask.mif ${yasmin}/nicotine/stats_FA/FA_ -force -nthreads 15 -nshuffles 5000

#mrclusterstats ${home}/input_files_ADC.txt ${yasmin}/DTI_stats/design_matrix_FD.txt ${yasmin}/DTI_stats/contrast_matrix_FD.txt ${yasmin}/padded_WM_mask.mif ${yasmin}/ADC_DTI_stats -force -nthreads 15 -nshuffles 5000

#mrclusterstats ${home}/input_files_AD.txt ${yasmin}/DTI_stats/design_matrix_FD.txt ${yasmin}/DTI_stats/contrast_matrix_FD.txt ${yasmin}/padded_WM_mask.mif ${yasmin}/AD_DTI_stats -force -nthreads 15 -nshuffles 5000

#mrclusterstats ${home}/input_files_RD.txt ${yasmin}/DTI_stats/design_matrix_FD.txt ${yasmin}/DTI_stats/contrast_matrix_FD.txt ${yasmin}/padded_WM_mask.mif ${yasmin}/RD_DTI_stats -force -nthreads 15 -nshuffles 5000
