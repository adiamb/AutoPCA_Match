# AutoPCA_Match
Matching cases and controls by principal components in a GWAS is required, but it is generally done by examining a PC1 Vs PC2 plot and pruning controls that are unlike cases ethnically. This script was written to solve this problem and of course out of my sheer laziness to do this by visual inspection. The script takes each case and iteratively calculates euclidean distance on all PCs wrt to all controls, The top n controls closest to the respective case are then selected.  
**A Typical Run Requires 3 Arguments**  
Argument1 = input.csv (First column should have IDs and second column should have diagnosis information 1- case 0- control)  
Argument2 = Number of controls per case (I typically set this at 5)  
Argument3 = Output prefix only  
```Rscript PCAmatch.R input.csv 5 output```
