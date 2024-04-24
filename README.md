# OutOfTheBag

### Overview

This repository is to accompany the manuscript **“Out of (the) Bag - encoding categorical predictors impacts out-of-bag samples”** by *HL Smith, PJ Biggs, NP French, ANH Smith,* and *JC Marshall* (2024).

**Out Of (the) Bag** details issues with using out-of-bag observations to calculate error rates and measures of variable importance when categorical variables are ordinal encoded.

This repository contains all the code for the random noise simulation studies described in the paper.

### Contents of this repository

The key contents are organised as follows:

-   OutOfTheBag
    -   README.Rmd
    -   OutOfTheBag.Rproj
    -   files

The R project is called `OutOfTheBag.Rproj`.

The directory `files` contains the file `out_of_the_bag_code`. This file contains code for two random-noise simulation studies. The first compares misclassification rates from out-of-bag samples *versus* independent test data. The second compares variable importance when predictor variables have been ordinal encoded using a target-based method *versus* a target-agnostic method. Five measures of variable importance are assessed in this way : Mean Decrease Accuracy, Holdout, Independent Holdout, Mean Decrease Impurity, and Actual Impurity Reduction. The code for the figures is also included.
