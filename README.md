# ieeetlt-orchestration-load
Preprocessing, analysis and visualization scripts for the IEEE TLT journal paper, "Orchestration Load Indicators and Patterns" by Prieto, Sharma &amp; Dillenbourg.

Basically, this is a set of R markdown reports and scripts to help reproduce the download of raw data (from Zenodo -- Study 1, Study 2, Study 3, Study 4), data pre-processing, analysis and visualizations, many of which are directly used in the paper. All these processes are tied together by reports for each case, and another cross-case analysis report in R Markdown, to be compiled using the knitr package.

## Requirements and Installation

In order to reproduce the paper's data analysis and generate the corresponding report, you need at least the following:

* An internet connection (to download the data)
* The ```curl``` tool (to download the data)
* R
* RStudio (optional, but very convenient)
* The ```knitr``` package

## Usage

Basically, open the desired .Rmd file in RStudio, and click on ```Knit HTML```. An HTML version of the report should be generated **(this may take some time, though!)**.

## Project structure

The project is organized in a folder structure, inspired by [ProjectTemplate](http://projecttemplate.net/):

* data/ : Contains the scripts to download, uncompress and load the raw data
* lib/ : Contains some useful R scripts used throughout the report, such as the functions to calculate rolling windows, extract extreme load episodes to be video coded, etc.
