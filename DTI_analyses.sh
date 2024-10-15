----------------------------------------------------------------------------------------------------------------
DTI PRE-PROCESSING
----------------------------------------------------------------------------------------------------------------

caselist=/DWIdata2023/harmonized_data/included/caselist.txt

case=`head -${SLURM_ARRAY_TASK_ID} ${caselist} | tail -1`
home=/DWIdata2023/harmonized_data/included
wd=${home}/${case}

#modules to load
module load MRtrix/3.0.4

dwiextract -singleshell -bzero -shells 0,1500 ${wd}/harmonized_${case}.mif ${wd}/${case}_b1500.mif -force

dwi2tensor -mask ${wd}/${case}_mask.mif ${wd}/${case}_b1500.mif ${wd}/${case}_DT.mif -force

tensor2metric -mask ${wd}/${case}_mask.mif -fa ${wd}/${case}_FA.mif ${wd}/${case}_DT.mif -force

mrtransform ${wd}/${case}_FA.mif -warp ${wd}/${case}_subject2template_warp_HC.mif ${wd}/${case}_FA_in_template_space.mif -force

----------------------------------------------------------------------------------------------------------------
VBA - WHOLE BRAIN
----------------------------------------------------------------------------------------------------------------

yasmin=/DWIdata2023/template/2024_stats

mrclusterstats ${home}/input_files_wholebrain_FA.txt ${yasmin}/wholebrain_FA/design_matrix_FD.txt ${yasmin}/nicotine/contrast_matrix_FD.txt ${yasmin}/padded_WM_mask.mif ${yasmin}/wholebrain_FA/stats_FA/FA_ -force -nthreads 15 -nshuffles 5000

----------------------------------------------------------------------------------------------------------------
VBA - NICOTINE
----------------------------------------------------------------------------------------------------------------

mrclusterstats ${home}/input_files_nicotine_FA.txt ${yasmin}/nicotine/design_matrix_FD.txt ${yasmin}/nicotine/contrast_matrix_FD.txt ${yasmin}/padded_WM_mask.mif ${yasmin}/nicotine/stats_FA/FA_ -force -nthreads 15 -nshuffles 5000

----------------------------------------------------------------------------------------------------------------
VBA - CANNABIS
----------------------------------------------------------------------------------------------------------------

mrclusterstats ${home}/input_files_cb_FA.txt ${yasmin}/cannabis/design_matrix_FD.txt ${yasmin}/cannabis/contrast_matrix_FD.txt ${yasmin}/padded_WM_mask.mif ${yasmin}/cannabis/stats_FA/FA_ -force -nthreads 15 -nshuffles 5000

----------------------------------------------------------------------------------------------------------------
TBSS - WHOLE BRAIN
----------------------------------------------------------------------------------------------------------------
home=/DWIdata2023/stats
caselist=/DWIdata2023/stats/caselist.txt
case=`head -${SLURM_ARRAY_TASK_ID} ${caselist} | tail -1`

cd ${home}/mytbss

caselist=`cat caselist.txt` 

for i in $caselist ; do mrconvert sub-${i}_FA.mif sub-${i}_FA.nii.gz ; do tbss_1_preproc sub-${i}_FA.nii.gz ; done 

tbss_2_reg -t ${home}/mytbss/templateHC/mean_FA.nii.gz 

tbss_3_postreg -S 

tbss_4_prestats 0.2 

cd FA
imglob *_FA.*

cd ../stats

randomise -i all_FA_skeletonised -o tbss -m mean_FA_skeleton_mask -d design.mat -t design.con -n 5000 --T2
