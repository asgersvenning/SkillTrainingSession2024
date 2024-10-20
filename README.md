# R: `mgcv` as a General Framework for (Most) Generalized Models

This repository contains the materials for a skill training session on using the R package `mgcv` to model various types of generalized models, including Generalized Linear Models (GLMs), Generalized Additive Models (GAMs), Generalized Linear Mixed Models (GLMMs), and Generalized Additive Mixed Models (GAMMs). The session is designed for early-career researchers and PhD students, aiming to unify these models under the `gam`/`bam` functions from `mgcv`.

## Overview

The session is structured around hands-on exercises using three datasets from the literature, with a focus on:

- Choosing the appropriate model family (`ziP`, `betar`, `gaullss`, etc.).
- Understanding and applying different smoothing functions.
- Efficiently using `gam`/`bam` for large datasets.
- Visualizing model predictions and diagnostics (e.g., marginal and residual plots, QQ plots).

### Contents

- **data/**: Contains the datasets used in the session.
- **tutorial.Rmd**: The RMarkdown tutorial that guides participants through the exercises.
- **results/**: Folder for where the results produced in the excercises are stored.
- **helpers/**: Helper functions for the tutorial.
- **development/**: Contains code for developing the tutorial. *(And a proposed solution of the tutorial by me)*

### Setup

To follow along with the tutorial, you will need the following R packages:

- `mgcv`
- `ggplot2`
- `dplyr`

You can install these packages using the following commands in RGui (with RStudio closed):

```r
install.packages(c("mgcv", "ggplot2", "dplyr", "rmdformats", "xfun"))
```

### How to Use the Tutorial

* (optional) Create a fork of this repository.
* Download or clone this repository (or the fork).
* Open tutorial.Rmd in RStudio.

Work through the tutorial, filling in the code where indicated and answering questions along the way.
Run the code blocks in RStudio to see the outputs, predictions, and diagnostics visualized in real-time.

(optional) If you forked the repository, you can push your changes to your fork to save your progress, and allow others - me included - to see your work.

### Contact
For any issues, feel free to open an issue in this repository or contact me at asgersvenning@ecos.au.dk.
