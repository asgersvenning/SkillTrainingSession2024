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
- **results/**: Folder for participants to save and upload their results after completing the exercises.

### Setup

To follow along with the tutorial, you will need the following R packages:

- `mgcv`
- `ggplot2`
- `dplyr`

You can install these packages using the following commands:

```r
install.packages(c("mgcv", "ggplot2", "dplyr"))
```

### How to Use the Tutorial
* Download/Clone this repository.
* Open tutorial.Rmd in RStudio.

Work through the tutorial, filling in the code where indicated and answering questions along the way.
Run the code blocks in RStudio to see the outputs, predictions, and diagnostics visualized in real-time.

### Contact
For any issues, feel free to open an issue in this repository or contact me at asgersvenning@ecos.au.dk.