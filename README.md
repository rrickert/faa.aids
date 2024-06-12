Load FAA ASIAS AIDS Data
================
Randall Rickert
2024-06-08

## Purpose

The purpose of this R package is the loading of the *Accident/Incident
Data System (AIDS)* dataset maintained and published by the United
States Federal Aviation Administration (FAA) for analysis in R. The
result is a dplyr::tibble, a dataframe-like object type I chose because
I am using the *tidyverse* metapackage for a case study of general
aviation accident/incident trends.

## Data Source

[The raw AIDS data](https://www.asias.faa.gov/apex/f?p=100:189:::NO),
updated at the end of each calendar month, is hosted on the FAA’s
[*Aviation Safety Information Analysis and Sharing (ASIAS)* web
site](https://www.asias.faa.gov/). This code was developed using the
March 30, 2024 update, which was the latest available at the time of
this writing.

The FAA distributes the data in a series of tab-delimited text files.
Data file base names begin with `a` or `e`, followed by the year span
represented in the file, in the format: `YYYY_YY`. For example,
`a2020_25.txt` contains aviation accident/incident observations from the
year 2020 through the year 2025 inclusively. The files beginning with
`a` contain 179 serially-named columns. Brief descriptions of the
columns are stored in a separate fixed-width text file:
`Afilelayout.txt`. The files beginning with `e` contain two columns: a
column of remarks and a column of unique keys corresponding to one of
the 179 columns in the `a` files, making it easy to naturally join the
`e` file remarks to the `a` file columns.

## Factor Codes

The FAA distributes tables of codes used for factor data in a single
word processor document: `aidcodes.docx`. This document’s Lack of
structure makes it difficult to extract the individual tables
programmatically, so I extracted each table manually to its own
tab-delimited text file.

I inferred the mapping of those code tables to the serially-named data
columns containing encoded values by comparing the data column
descriptions in `Afilelayout.txt` with the table descriptions in
`aidcodes.docx`, but not all data columns appear in `Afilelayout.txt`
with useful descriptions.

Some of the encoded columns have corresponding decoded columns,
identified by descriptions in `Afilelayout.txt` ending with the word
*text*, allowing me to verify my code-table-to-column mappings by
checking the values I decoded against values from the corresponding
decoded *text* columns. However, not all encoded columns have
corresponding *text* columns, nor are the existing *text* columns
completely populated.

## Use

If you have the *devtools* package installed, you can install this
package directly from GitHub with:
`devtools::install_github("rrickert/faa.aids")`. The library exposes a
single function, `faa.aids::load_aids()`, which will by default load the
AIDS data packaged with this library. Optional arguments can be used to
specify an alternate path to FAA AIDS data files and regular expression
patterns to match the file names.

``` r
# Load the AIDS data packaged with this library
library(faa.aids)
data <- load_aids()
```

## Exploration

### Event Type and Date

Event dates span roughly the eighty years since the Second World War,
but the data are very sparse until 1973, which probably marks a dramatic
change in aviation record-keeping practices in the U.S. Another change
appears to be the inclusion of incidents other than accidents beginning
around 1978. Data from 2008 to present include only very preliminary
data. Yearly spikes suggest that the events follow strong seasonal
cycles, with a notable surge in 2020.

``` r
library(tidyverse)
data |>
  filter(
    `event date` >= as.Date("1968-01-01")
  ) |>
  ggplot(
    mapping = aes(x = `event date`, fill = `event type`)
  ) + geom_histogram(binwidth = 7)
```

![](README_files/figure-gfm/event%20histogram-1.png)<!-- -->

### Flying Type and Fatalities

From the twenty year span with the most complete data (1988 - 2008), we
can see that some types of aviation, such as crop dusting, kill people
with more seasonality than do commercial passenger or cargo flights.
Drug smuggling and aircraft thievery data are too sparse to draw a
conclusion.

``` r
data |>
  filter(
    between(`event date`, as.Date("1988-01-01"), as.Date("2007-12-31"))
  ) |>
  mutate(
    `flying type` = fct_lump_min(`flying type`, 5) # Declutter dirty data
  ) |>
  ggplot(
    mapping = aes(
      x = `event date`, y = `flying type`,
      alpha = `total deaths`, color = `flying type`
    )
  ) + guides(color = "none") + geom_jitter()
```

![](README_files/figure-gfm/flying%20type%20scatterplot-1.png)<!-- -->

### Summary Statistics

While the observations spanning 1973 through 2008 are relatively more
complete, the completion rates for indivitual columns vary from 0.0% (no
value is available for any observation) to 100.0% (a value is available
for every observation). From the 180 columns in the raw dataset, this
package returns forty columns that appear interesting and complete
enough to be useful for my own case study.

The *skimr* package provides a convenient summary.

``` r
# Summarize the resulting tibble
library(skimr)
skim(data)
```

|                                                  |        |
|:-------------------------------------------------|:-------|
| Name                                             | data   |
| Number of rows                                   | 229472 |
| Number of columns                                | 40     |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |        |
| Column type frequency:                           |        |
| character                                        | 28     |
| Date                                             | 1      |
| difftime                                         | 1      |
| factor                                           | 7      |
| numeric                                          | 3      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |        |
| Group variables                                  | None   |

Data summary

**Variable type: character**

| skim_variable                  | n_missing | complete_rate | min |   max | empty | n_unique | whitespace |
|:-------------------------------|----------:|--------------:|----:|------:|------:|---------:|-----------:|
| id                             |         0 |          1.00 |  15 |    15 |     0 |   227239 |          0 |
| city                           |      2158 |          0.99 |   1 |    15 |     0 |    20479 |          0 |
| latitude                       |    167509 |          0.27 |   1 |     6 |     0 |     5378 |          0 |
| longitude                      |    167559 |          0.27 |   1 |     6 |     0 |     7735 |          0 |
| aircraft registration          |         1 |          1.00 |   1 |     6 |     0 |   165871 |          0 |
| aircraft make                  |      2048 |          0.99 |   2 |    30 |     0 |     4848 |          0 |
| aircraft model                 |      1940 |          0.99 |   1 |    27 |     0 |     9936 |          0 |
| primary cause factor           |    108296 |          0.53 |   5 |    45 |     0 |      299 |          0 |
| flight phase                   |      9235 |          0.96 |   4 |    23 |     0 |       81 |          0 |
| damage                         |     29941 |          0.87 |   4 |    20 |     0 |       21 |          0 |
| cause category                 |     55447 |          0.76 |   1 |    20 |     0 |       18 |          0 |
| flying type                    |     47476 |          0.79 |   5 |    20 |     0 |       25 |          0 |
| secondary flying type          |     49858 |          0.78 |   5 |    20 |     0 |       37 |          0 |
| flying condition               |     78835 |          0.66 |   3 |    20 |     0 |       10 |          0 |
| secondary flying condition     |     53288 |          0.77 |   3 |    20 |     0 |       29 |          0 |
| light condition                |     53112 |          0.77 |   3 |    20 |     0 |       12 |          0 |
| weight class                   |     31142 |          0.86 |   4 |    20 |     0 |        7 |          0 |
| wing type                      |     31144 |          0.86 |   4 |    29 |     0 |       23 |          0 |
| power class                    |     31144 |          0.86 |   3 |    29 |     0 |        9 |          0 |
| engine power                   |    152976 |          0.33 |   7 |    20 |     0 |        9 |          0 |
| engine type                    |    152984 |          0.33 |   1 |    29 |     0 |       20 |          0 |
| landing gear type              |    141246 |          0.38 |   2 |    29 |     0 |       24 |          0 |
| additional cause factor        |    200893 |          0.12 |   5 |    20 |     0 |       41 |          0 |
| second additional cause factor |    222749 |          0.03 |   6 |    20 |     0 |       35 |          0 |
| supporting factor A            |    111060 |          0.52 |   5 |    30 |     0 |       28 |          0 |
| supporting factor B            |    177335 |          0.23 |   5 |    30 |     0 |       25 |          0 |
| pilot died                     |    140427 |          0.39 |   1 |     1 |     0 |        2 |          0 |
| remark                         |     16498 |          0.93 |   1 | 32317 |     0 |   208659 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| event date    |         0 |             1 | 1945-07-23 | 2024-03-05 | 1991-04-14 |    18627 |

**Variable type: difftime**

| skim_variable | n_missing | complete_rate | min    | max        | median   | n_unique |
|:--------------|----------:|--------------:|:-------|:-----------|:---------|---------:|
| local time    |     30153 |          0.87 | 0 secs | 86340 secs | 14:00:00 |     1436 |

**Variable type: factor**

| skim_variable   | n_missing | complete_rate | ordered | n_unique | top_counts                                     |
|:----------------|----------:|--------------:|:--------|---------:|:-----------------------------------------------|
| event type      |         0 |          1.00 | FALSE   |        2 | Acc: 119674, Inc: 109798                       |
| FAR part        |     27827 |          0.88 | FALSE   |       19 | GEN: 156512, OPE: 17479, OPE: 14309, AGR: 7168 |
| region          |       476 |          1.00 | FALSE   |       19 | SOU: 36568, GRE: 34152, WES: 33312, SOU: 32479 |
| district office |       648 |          1.00 | FALSE   |      271 | ORL: 5769, SCO: 5571, DEN: 5555, ANC: 5405     |
| state           |       771 |          1.00 | FALSE   |      170 | CAL: 24297, TEX: 17007, FLO: 15478, ALA: 11306 |
| accident type   |     50236 |          0.78 | FALSE   |       86 | SYS: 13473, ENG: 12733, COL: 11957, WHE: 11091 |
| flight plan     |     79221 |          0.65 | FALSE   |       15 | NON: 79214, INS: 31069, UNK: 24475, VIS: 13845 |

**Variable type: numeric**

| skim_variable  | n_missing | complete_rate | mean |   sd |  p0 | p25 | p50 | p75 | p100 | hist  |
|:---------------|----------:|--------------:|-----:|-----:|----:|----:|----:|----:|-----:|:------|
| total deaths   |         0 |          1.00 | 0.15 | 1.58 |   0 |   0 |   0 |   0 |  260 | ▇▁▁▁▁ |
| engine count   |     31176 |          0.86 | 1.28 | 0.58 |   0 |   1 |   1 |   2 |    4 | ▁▇▂▁▁ |
| total injuries |         0 |          1.00 | 0.21 | 1.24 |   0 |   0 |   0 |   0 |  200 | ▇▁▁▁▁ |
