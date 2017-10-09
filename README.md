# Extract nationwide spatial data from OSM

Give the script a csv of key-value pairs, and return a csv of all matched to that key-value par for the entire US. Output contains WKT geometry field.

***

- Clone repo:
```{bash}
$ git clone https://github.com/simonkassel/us-osm-data/
```

## Run from R IDE:

- Set working directory to local version of repo
- Source functions:
```{r}
source('get_osm.R')
```
- Place key-value csv in working directory
- Pull from OSM api and write to csv:
```{r}
csv_to_list(path = '[input csv file path]', keys = '[name of key field in csv]', values = '[name of value field in csv]', query = TRUE)
```
- Script will write output csv to working directory with modified filename of input dataset
- For more flexible functionality, check individual function documentation within Script

## Run from command line
- Sample command
```{bash}
$ Rscript get_osm.R [input csv] [key field] [value field]
```
- Find output csv in same directory as input
