# R Shiny Example App

Run the application in R with:

```R
shiny::runApp('rshiny_app')
```

The app uses **shiny.router** for multi-page navigation via links like `#/accueil` or `#/financier`. Routes are declared with `router_ui()` and registered on the server with `router_server()`.

Datasets are loaded from `rshiny_app/data/Base0.xlsx`. The home page displays an image carousel powered by the **slickR** package when available; otherwise static images are shown.
