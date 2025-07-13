# R Shiny Example App

Run the application in R with:

```R
shiny::runApp('rshiny_app')
```

The app now uses **shiny.router** to provide a modern multi-page navigation bar. Pages are accessible via links like `#/accueil` or `#/financier` without reloading the whole interface.

Datasets are loaded from `rshiny_app/data/Base0.xlsx`. The home page displays an image carousel powered by the **slickR** package when available; otherwise static images are shown.
