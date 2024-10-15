File path format


DWIdata2023
  included
    caselist.txt
    sub-x
      fixel_in_template_space_HC
        directions.mif
        fd_HC.mif
        index.mif
      fixel_in_template_space_HCPT
        directions.mif
        fd_HCPT.mif
        index.mif
      fixel_in_template_space_NOT_REORIENTED_HC
        directions.mif
        fd_HC.mif
        index.mif
      fixel_in_template_space_NOT_REORIENTED_HCPT
        directions.mif
        fd_HCPT.mif
        index.mif
      fod_in_template_space_NOT_REORIENTED_HC.mif
      fod_in_template_space_NOT_REORIENTED_HCPT.mif
      harmonized_sub-XXXX_b0_unweighted.nii.gz
      harmonized_sub-XXXX.bvals
      harmonized_sub-XXXX.bvecs
      harmonized_sub-XXXX.mif
      harmonized_sub-XXXX_ses-1_dir-416_desc-XcUnEdEp_dwi.nii.gz
      sub-XXXX_b1500.mif
      sub-XXXX_csf_fod.mif
      sub-XXXX_csffod_norm.mif
      sub-XXXX_csf.txt
      sub-XXXX_DT.mif
      sub-XXXX_dwi_mask_in_template_space_HC.mif
      sub-XXXX_dwi_mask_in_template_space_HCPT.mif
      sub-XXXX_FA_in_template_space.mif
      sub-XXXX_FA.mif
      sub-XXXX_gm_fod.mif
      sub-XXXX_gmfod_norm.mif
      sub-XXXX_gm.txt
      sub-XXXX_mask_BET_mask.nii.gz
      sub-XXXX_mask_BET.nii.gz
      sub-XXXX_mask.mif
      sub-XXXX_subject2template_warp_HC.mif
      sub-XXXX_subject2template_warp_HCPT.mif
      sub-XXXX_template2subject_warp_HC.mif
      sub-XXXX_template2subject_warp_HCPT.mif
      sub-XXXX_wm_fod.mif
      sub-XXXX_wmfod_norm.mif
      sub-XXXX_wm.txt
  newmasks   
  template
    fod_inputHC_RISH
    mask_inputHC_RISH
    resultHC_RISH
    fd
    fdc
    fc
    matrix
    stats_fd_RISH
    stats_fc_RISH
    stats_fdc_RISH
    tracks_20_million.tck
    tracks_20_million_sift.tck
    tracks_200k_sift.tck
    wmfod_template.mif
    files.txt
    design_matrix_FD.txt
    design_matrix_FC.txt
    contrast_matrix_FD.txt
    contrast_matrix_FC.txt
  scripts
    output
      out_o.%A_%a
      err_e.%A_%a


----------------------------------------------------------------------------------------------------------------
PRE-PROCESSING
----------------------------------------------------------------------------------------------------------------

# Define the caselist file path
caselist=/path/to/data/caselist.txt

home=/data/gpfs/projects/punim1958/DWIdata2023/newmasks

echo ${case}

module load fsl/6.0.1-python-3.7.4

fslroi ${home}/${case}*.nii.gz ${home}/${case}_b0_unweighted 0 1

bet ${home}/${case}_b0_unweighted ${home}/${case}_mask_BET -m -R -f 0.2 -g -0.05


while read line; do
mrconvert ${home}/${line}/harmonized_${line}*_dwi.nii.gz ${home}/${line}/harmonized_${line}.mif -fslgrad ${home}/${line}/harmonized_${line}*_dwi.bvec ${home}/${line}/harmonized_${line}*_dwi.bval -force
done < ${home}/caselist.txt

------------------------------------------------------------------------------------------------------------------
CSD MODELLING
------------------------------------------------------------------------------------------------------------------

caselist=/data/gpfs/projects/punim1958/DWIdata2023/harmonized_data/included/caselist.txt

# the following two lines selects the case from caselist and stores it into case
echo ${SLURM_ARRAY_TASK_ID}
case=`head -${SLURM_ARRAY_TASK_ID} ${caselist} | tail -1`
home=/data/gpfs/projects/punim1958/DWIdata2023/harmonized_data/included

echo ${case}

#modules to load
module load MRtrix/3.0.4

#create a working directory
wd=${home}/${case}

#derives gm, wm and csf responses
dwi2response dhollander ${wd}/harmonized_${case}*_dwi.nii.gz ${wd}/${case}_wm.txt ${wd}/${case}_gm.txt ${wd}/${case}_csf.txt -fslgrad ${wd}/harmonized_${case}*.bvec ${wd}/harmonized_${case}*.bval -mask ${wd}/${case}*_mask.nii.gz

dwi2fod msmt_csd ${wd}/harmonized_${case}.mif ${wd}/${case}_wm.txt ${wd}/${case}_wm_fod.mif ${wd}/${case}_gm.txt ${wd}/${case}_gm_fod.mif  ${wd}/${case}_csf.txt ${wd}/${case}_csf_fod.mif -mask ${wd}/${case}_mask.mif -force

mtnormalise ${wd}/${case}_wm_fod.mif ${wd}/${case}_wmfod_norm.mif ${wd}/${case}_gm_fod.mif ${wd}/${case}_gmfod_norm.mif ${wd}/${case}_csf_fod.mif ${wd}/${case}_csffod_norm.mif -mask ${wd}/${case}_mask.mif -force

----------------------------------------------------------------------------------------------------------------
TEMPLATE
----------------------------------------------------------------------------------------------------------------

# Define the case list
caselist=(1028 1064 1067 1080 1084 1091 2004 2046 2049 3001 3026 3028 3029 3036 4003 4006 4018 4055 4078 4119)
home=/data/gpfs/projects/punim1958/DWIdata2023

# Define the output directories
mkdir -p ${home}/template/fod_inputHC_RISH
mkdir -p ${home}/template/mask_inputHC_RISH
mkdir -p ${home}/template/resultHC_RISH

# Load the required module
module load MRtrix/3.0.4

# Process the cases
for case in ${caselist[@]}; do
  echo "Processing case: ${case}"

  wd=${home}/harmonized_data/included/sub-${case}
  
# Link the normalised FOD image and upsampled mask to the template directory
  ln -sr ${wd}/sub-${case}_wmfod_norm.mif ${home}/template/fod_inputHC_RISH/sub-${case}_wmfod_norm.mif
  ln -sr ${wd}/sub-${case}_mask.mif ${home}/template/mask_inputHC_RISH/sub-${case}_mask.mif
done

# Run the population template script
population_template ${home}/template/fod_inputHC_RISH -mask_dir ${home}/template/mask_inputHC_RISH ${home}/template/resultHC_RISH/wmfod_template.mif -voxel_size 1.5

------------------------------------------------------------------------------------------------------------------
REGISTER TO TEMPLATE SPACE
------------------------------------------------------------------------------------------------------------------
case=`head -${SLURM_ARRAY_TASK_ID} ${caselist} | tail -1`
home=/data/gpfs/projects/punim1958/DWIdata2023

echo ${case}

module load MRtrix/3.0.4

wd=${home}/harmonized_data/included/${case}

mrregister ${wd}/${case}_wmfod_norm.mif -mask1 ${wd}/${case}_mask.mif ${home}/template/resultHC_RISH/wmfod_template.mif -nl_warp ${wd}/${case}_subject2template_warp_HC.mif ${wd}/${case}_template2subject_warp_HC.mif -force

mrtransform ${wd}/${case}_mask.mif -warp ${wd}/${case}_subject2template_warp_HC.mif -interp nearest -datatype bit ${wd}/${case}_dwi_mask_in_template_space_HC.mif -force

mrmath ${home}/harmonized_data/included/*/*_dwi_mask_in_template_space_HC.mif min ${home}/template/resultHC_RISH/template_mask.mif -datatype bit

mrtransform ${wd}/${case}_wmfod_norm.mif -warp ${wd}/${case}_subject2template_warp_HC.mif -reorient_fod no ${wd}/fod_in_template_space_NOT_REORIENTED_HC.mif

------------------------------------------------------------------------------------------------------------------
FIXEL MASK
------------------------------------------------------------------------------------------------------------------

fod2fixel -mask ${home}/template/resultHC_RISH/template_mask.mif ${wd}/fod_in_template_space_NOT_REORIENTED_HC.mif ${wd}/fixel_in_template_space_NOT_REORIENTED_HC -afd fd_HC.mif

------------------------------------------------------------------------------------------------------------------
METRIC COMPUTATION AND TRACTOGRAPHY
------------------------------------------------------------------------------------------------------------------

fixelreorient ${wd}/fixel_in_template_space_NOT_REORIENTED_HC ${wd}/${case}_subject2template_warp_HC.mif ${wd}/fixel_in_template_space_HC

fixelcorrespondence ${wd}/fixel_in_template_space_HC/fd_HC.mif ${home}/template/resultHC_RISH/fixel_mask ${home}/template/resultHC_RISH/fd ${case}_fd_HC.mif

warp2metric ${wd}/${case}_subject2template_warp_HC.mif -fc ${home}/template/resultHC_RISH/fixel_mask ${home}/template/resultHC_RISH/fc ${case}_HC.mif -force

tckgen -angle 22.5 -maxlen 250 -minlen 10 -power 1.0 ${home}/template/resultHC_RISH/wmfod_template.mif -seed_image ${home}/template/resultHC_RISH/template_mask.mif -select 20000000 -cutoff 0.06 ${home}/template/resultHC_RISH/tracks_20_million.tck

tcksift ${home}/template/resultHC_RISH/tracks_20_million.tck ${home}/template/resultHC_RISH/wmfod_template.mif ${home}/template/resultHC_RISH/tracks_20_million_sift.tck -term_number 2000000

fixelconnectivity ${home}/template/resultHC_RISH/fixel_mask/ ${home}/template/resultHC_RISH/tracks_20_million_sift.tck ${home}/template/resultHC_RISH/matrix/

fixelfilter ${home}/resultHC_RISH/fd smooth ${home}/resultHCPT_RISH/fd_smooth -matrix ${home}/resultHCPT_RISH/matrix/
fixelfilter ${home}/resultHC_RISH/fdc smooth ${home}/resultHCPT_RISH/fdc_smooth -matrix ${home}/resultHCPT_RISH/matrix/
fixelfilter ${home}/resultHC_RISH/log_fc smooth ${home}/resultHCPT_RISH/log_fc_smooth -matrix ${home}/resultHCPT_RISH/matrix/

fixelfilter ${home}/resultHCPT_RISH/fd smooth ${home}/resultHCPT_RISH/fd_smooth -matrix ${home}/resultHCPT_RISH/matrix/
fixelfilter ${home}/resultHCPT_RISH/fdc smooth ${home}/resultHCPT_RISH/fdc_smooth -matrix ${home}/resultHCPT_RISH/matrix/
fixelfilter ${home}/resultHCPT_RISH/log_fc smooth ${home}/resultHCPT_RISH/log_fc_smooth -matrix ${home}/resultHCPT_RISH/matrix/

------------------------------------------------------------------------------------------------------------------
STATISTICAL INFERENCE
------------------------------------------------------------------------------------------------------------------

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

home=/data/gpfs/projects/punim1958/DWIdata2023/template

#modules to load
module load MRtrix/3.0.4

#whole brain statistical analysis of FD,FC,FDC
# -mask flag uses voxel-derived mask to restrict analysis to WM due to fixels in ventricles. -nthreads flag enables multi-threading as command will not run on 
#single core - should run around 5-6 hours using 20 cores
#-f flag for f statistics

fixelcfestats -mask ${home}/resultHC_RISH/wmfixel_mask/directions.mif ${home}/resultHC_RISH/fd_smooth ${home}/2024_stats/wholewm_stats/files.txt ${home}/2024_stats/wholewm_stats/design_matrix_FD.txt ${home}/2024_stats/wholewm_stats/contrast_matrix_FD.txt ${home}/resultHC_RISH/matrix/ ${home}/2024_stats/wm_mask_stats/stats_fd_RISH -force -nthreads 20

fixelcfestats -mask ${home}/2024_stats/wm_fixel_mask/wm_fixel_mask.mif ${home}/resultHC_RISH/log_fc_smooth ${home}/2024_stats/wholewm_stats/files.txt ${home}/2024_stats/wholewm_stats/design_matrix_FC.txt ${home}/2024_stats/wholewm_stats/contrast_matrix_FC.txt ${home}/resultHC_RISH/matrix/ ${home}/2024_stats/wm_mask_stats/stats_logfc_RISH/ -force -nthreads 20

fixelcfestats -mask ${home}/2024_stats/wm_fixel_mask/wm_fixel_mask.mif ${home}/resultHC_RISH/fdc_smooth ${home}/2024_stats/wholewm_stats/files.txt ${home}/2024_stats/wholewm_stats/design_matrix_FC.txt ${home}/2024_stats/wholewm_stats/contrast_matrix_FC.txt ${home}/resultHC_RISH/matrix/ ${home}/2024_stats/wm_mask_stats/stats_fdc_RISH/ -force -nthreads 20

------------------------------------------------------------------------------------------------------------------
VISUALISATION
------------------------------------------------------------------------------------------------------------------
#create significant mask
mrthreshold fwe_pvalue.mif -abs 0.95 binary_sig_mask_output.mif

#Calculate percentage change 
#beta1 correlates to 2nd row with HCs 
mrcalc abs_effect.mif beta1.mif -div 100 -mult percentage_effect.mif 

#to view results mapped to tractography
#load tractography, threshold by fwe_pvalue.tsf 0.95, colour by percentage_effect.tsf
fixel2tsf fixel_input.mif tractography.tck output_track_file.tsf
