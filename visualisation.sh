------------------------------------------------------------------------------------------------------------------
VISUALISATION
------------------------------------------------------------------------------------------------------------------

# Create a significant mask
mrthreshold fwe_pvalue.mif -abs 0.95 binary_sig_mask_output.mif

# Calculate percentage change
mrcalc abs_effect.mif beta1.mif -div 100 -mult percentage_effect.mif 

# To view results mapped to tractography
# Load tractography, threshold by fwe_pvalue.tsf 0.95, colour by percentage_effect.tsf
fixel2tsf fixel_input.mif tractography.tck output_track_file.tsf


