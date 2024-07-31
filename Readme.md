Requirements
------------

The scripts were written for R 3.6.1 and require the following 
libraries:

    tidyverse
    ggplot2

Quickstart
----------

To generate the keyword trending plot and Gephi input files run:

    Rscript ScopusSearch.R

This creates a `KeywordTrend.png` plot and `GephiAuthor.csv` 
`GephiListAuthorLastYear.csv` files.
    
To generate the subject area boxplot run:

    Rscript CitationCode.R

This creates a `SubjectBoxplot.png` file.

