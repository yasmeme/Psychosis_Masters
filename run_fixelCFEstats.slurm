#!/bin/bash
#SBATCH -J base_stats
#SBATCH -a 1
#SBATCH --time=50:00:00
#SBATCH -o output/out_yasmin2.%A_%a
#SBATCH -e output/err_yasmin2.%A_%a
#SBATCH --mem=400000
#SBATCH --cpus-per-task=20

# the following code prints the date
start=`date +%s`
echo ${start}

# the following two lines selects the case from caselist and stores it into case

echo ${SLURM_ARRAY_TASK_ID}

home=/DWIdata2023/template

#modules to load
module load MRtrix/3.0.4

#whole brain statistical analysis of FD,FC,FDC

# -mask flag uses voxel-derived mask to restrict analysis to WM due to fixels in#ventricles. -nthreads flag enables multi-threading as command will not run on 
#single core - should run around 5-6 hours using 20 cores
#-f flag for f statistics

fixelcfestats -mask ${home}/resultHC_RISH/wmfixel_mask/directions.mif ${home}/resultHC_RISH/fd_smooth ${home}/2024_stats/wholewm_stats/files.txt ${home}/2024_stats/wholewm_stats/design_matrix_FD.txt ${home}/2024_stats/wholewm_stats/contrast_matrix_FD.txt ${home}/resultHC_RISH/matrix/ ${home}/2024_stats/wm_mask_stats/stats_fd_RISH -force -nthreads 20

#fixelcfestats -mask ${home}/2024_stats/wm_fixel_mask/wm_fixel_mask.mif ${home}/resultHC_RISH/log_fc_smooth ${home}/2024_stats/wholewm_stats/files.txt ${home}/2024_stats/wholewm_stats/design_matrix_FC.txt ${home}/2024_stats/wholewm_stats/contrast_matrix_FC.txt ${home}/resultHC_RISH/matrix/ ${home}/2024_stats/wm_mask_stats/stats_logfc_RISH/ -force -nthreads 20

#fixelcfestats -mask ${home}/2024_stats/wm_fixel_mask/wm_fixel_mask.mif ${home}/resultHC_RISH/fdc_smooth ${home}/2024_stats/wholewm_stats/files.txt ${home}/2024_stats/wholewm_stats/design_matrix_FC.txt ${home}/2024_stats/wholewm_stats/contrast_matrix_FC.txt ${home}/resultHC_RISH/matrix/ ${home}/2024_stats/wm_mask_stats/stats_fdc_RISH/ -force -nthreads 20
