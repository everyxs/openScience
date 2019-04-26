1. genderDetect.R
* Input files: "newdataCombined.csv"
* Output files: "OpenSci3.csv", "SinglePie.pdf", "MultiPie.pdf"

***

Once you are inside the Jupyter Lab environment. Navigate to the code-data folder and open the "genderDetect.ipynb" notebook.

The first 3 cells of the notebook read papers from "newdataCombined.csv" within the 2010-2017 range. Then we we use "Year2" for birth year estimate, i.e. assuming authors are in the age range [35-55] when they publish. 

Next 2 cells break the author list into names and further into name parts for gender detection. We use the [gender package]( https://github.com/ropensci/gender) in R (Lincoln Mullen (2015)) to predict the probability of each name part (in reversed order, i.e. last name first) being female. We exclude parts representing initials. Authors with probability over 0.5 are labeled “female” and those with probability below 0.5 are labeled “male”.  Authors with no usable name parts are labeled “unknown”.  

The gender labeled data is then aggregated back to paper level according to author orders and reorganized as "OpenSci3.csv" for further statistical analysis. We also group the paper level data into categories in preparation for plotting. The last 4 cells reorganize the data and uses [ggplot2](https://ggplot2.tidyverse.org/) to generate pie charts in Figure 2 of the paper.
![](https://github.com/iuni-cadre/ReproducibilityDemo/blob/master/code-data/MultiPie.png)