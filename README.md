# DELCODE_agitationLC
Locus coeruleus signal intensity and emotion regulation in agitation in Alzheimer’s disease

This repository contains the R code used to analyze data obtained from the German Center for Neurodegenerative Diseases (DZNE)-Longitudinal Cognitive Impairment and Dementia (DELCODE) study.
The DELCODE study data are not publicly available, but may be provided from the DELCODE study committee based on individual data sharing agreements (see https://www.dzne.de/en/research/studies/clinical-studies/delcode/). 

## Running the code

You will need to install [R](http://cran.rstudio.com/), [Rstudio](http://www.rstudio.com/ide/download/) and Lavaan. To install Lavaan, open Rstudio and run the commands:


```r
install.packages("lavaan", dependencies=TRUE)
library(lavaan)
```

Next, you need to import data by changing the working directory to the data you would like to analyze. 

