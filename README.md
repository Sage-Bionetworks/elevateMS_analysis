# elevateMS_analysis

Analysis of elevateMS. 

## Feature Extraction

These are the steps and dependencies required to run the walking, tapping, rest, and tremor feature extractions.

### Libraries

Most of the libraries needed can be installed via R's `install.packages()` or via bioconductor `source("https://bioconductor.org/biocLite.R"); biocLite("packageName")`.

Two of the packages (`mpowertools` and `githubr`) need to be installed via `devtools`:
```
library(devtools) 
devtools::install_github("Sage-Bionetworks/mpowertools")
devtools::install_github("brian-bot/githubr")
```

**Please note that these scripts use the old R Synapse Client and not synapser.**

### Steps to Run

It's recommended that you run this on an EC2 instance due to the large amount of data it has to process. As of April 5, 2018, the data has been run on an Amazon m4.2xlarge instance with [Louis Aslett's RStudio AMI](http://www.louisaslett.com/RStudio_AMI/). **You MUST create a new password for logging into your instance if using this AMI!**

 1. Spin up an EC2 instance with the RStudio AMI
 2. Open RStudio 
 2. Click the "Terminal" tab in RStudio. Clone the repository:
    ```
    git clone https://github.com/Sage-Bionetworks/elevateMS_analysis.git
    ```
 3. Open the elevateMS_analysis.Rproj file
 4. Navigate to the feature you'd like to extract (e.g. `restFeatures.R` for rest features) and run the code
 5. Run all the extractions of interest
 6. Stop your instance
 7. Ta-da, you're done!
