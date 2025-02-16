#!/bin/bash

# Convert each tract ROI from TractSeg into a tract fixel mask
for i in $tract ; do 
    tck2fixel "${i}.tck" ../../fixel_mask . "${i}_fx.mif" 
done

# Threshold the mask to make binary
for i in $tract ; do 
    mrthreshold fixel_tracts/"${i}_fx.mif" -abs 0.1 tract_fx_mask/"${i}_fx_mask.mif" 
done

# Count significant fixels in each binary fixel tract mask
for i in $tract ; do 
    count=$(mrstats -ignorezero -mask "${i}_fx_mask.mif" -output count "${i}_fx.mif")
    echo "${i},$count" >> ../tract_full_count.txt 
done
