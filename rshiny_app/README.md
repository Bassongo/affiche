# R Shiny Example App

This repository now contains a minimal Shiny application. Run the app using `shiny::runApp('rshiny_app')` in R.

The datasets are loaded from `rshiny_app/data/PUDC_SUIVI TRIMESTRE  DU PTBA 2025 VF.xlsx`.

The home page uses the **slickR** package to display an image carousel. The app now detects if this package is missing and falls back to static images. For the full carousel experience install slickR with:

```R
install.packages("slickR")
```
