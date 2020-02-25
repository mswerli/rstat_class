## Shiny app for plotting stat cast data

### Shiny App
App can be run using `shiny_app.R`

UI and Server definitions are in the `shiny_app` directory

### Data Retrieval and Transformation

Occur in classes in the `classes` dir

Pitcher and hitter classes share a super class (`data_fetcher.R`) with a set of methods for getting and tranforming data.

### Data Source

Data is from baseballsavant.com and appac.github.io/mlb-data-api-docs/ . Be reasonable when pulling data from these sites


