#!/bin/bash
#SBATCH -J stats
#SBATCH -a 1-279
#SBATCH --time=05:00:00
#SBATCH --mem=200000
#SBATCH --cpus-per-task=4
#SBATCH -o output/out_FA.%A_%a
#SBATCH -e output/err_FA.%A_%a

caselist=/DWIdata2023/harmonized_data/included/caselist.txt

# the following code prints the date
start=`date +%s`
echo ${start}

# the following two lines selects the case from caselist and stores it into case

echo ${SLURM_ARRAY_TASK_ID}

case=`head -${SLURM_ARRAY_TASK_ID} ${caselist} | tail -1`
home=/DWIdata2023/harmonized_data/included
wd=${home}/${case}

echo ${case}

#modules to load
module load MRtrix/3.0.4


dwiextract -singleshell -bzero -shells 0,1500 ${wd}/harmonized_${case}.mif ${wd}/${case}_b1500.mif -force

dwi2tensor -mask ${wd}/${case}_mask.mif ${wd}/${case}_b1500.mif ${wd}/${case}_DT.mif -force

tensor2metric -mask ${wd}/${case}_mask.mif -fa ${wd}/${case}_FA.mif -adc ${wd}/${case}_ADC.mif -ad ${wd}/${case}_AD.mif -rd ${wd}/${case}_RD.mif ${wd}/${case}_DT.mif -force

mrtransform ${wd}/${case}_FA.mif -warp ${wd}/${case}_subject2template_warp_HC.mif ${wd}/${case}_FA_in_template_space.mif -force

mrtransform ${wd}/${case}_AD.mif -warp ${wd}/${case}_subject2template_warp_HC.mif ${wd}/${case}_AD_in_template_space.mif -force

mrtransform ${wd}/${case}_ADC.mif -warp ${wd}/${case}_subject2template_warp_HC.mif ${wd}/${case}_ADC_in_template_space.mif -force

mrtransform ${wd}/${case}_RD.mif -warp ${wd}/${case}_subject2template_warp_HC.mif ${wd}/${case}_RD_in_template_space.mif -force

