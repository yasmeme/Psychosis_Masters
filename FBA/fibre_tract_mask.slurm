#!/bin/bash

#SBATCH -o output/out_o.%A_%a
#SBATCH -e output/err_e.%A_%a
#SBATCH --mem=500G
caselist=/data/gpfs/projects/punim1958/DWIdata2023/scripts/caselist.txt

# the following code prints the date
start=`date +%s`
echo ${start}

# the following two lines selects the case from caselist and stores it into case

echo ${SLURM_ARRAY_TASK_ID}
case=`head -${SLURM_ARRAY_TASK_ID} ${caselist} | tail -1`
home=/data/gpfs/projects/punim1958/DWIdata2023/nonupsampling

echo ${case}

#modules to load
module load mrtrix/3.0.3-python-3.7.4
module load TractSeg/2.8

sh2peaks ${home}/${case}/${case}_wmfod_norm.mif ${home}/${yasmin}/${case}/${case}_peaks.mif

TractSeg -i ${home}/${yasmin}/${case}/${case}_peaks.mif --output_type tract_segmentation
TractSeg -i ${home}/${yasmin}/${case}/${case}_peaks.mif --output_type endings_segmentation
TractSeg -i ${home}/${yasmin}/${case}/${case}_peaks.mif --output_type TOM 
Tracking -i ${home}/${yasmin}/${case}/${case}_peaks.mif

tck2fixel [ options ] tracks_2_million_sift.tck/fixel_mask tck2fixel_output tck2fixel_output.mif




