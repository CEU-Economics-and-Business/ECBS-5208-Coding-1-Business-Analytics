# ECBS-5208-Coding-1-Business-Analytics
ECBS5208 - Coding 1: Data Management and Analysis with R for Business Analytics Track 2021/22 Fall.

This is the repository of the [Coding 1: Data Management and Analysis with R](https://courses.ceu.edu/courses/2021-2022/coding-1-data-management-and-analysis-r)
course in the 2021/2022 Fall term, part of the [MSc in Business Analytics](https://economics.ceu.edu/program/master-science-business-analytics) at CEU.

## Syllabus

Please find it in the `Syllabus` folder of this repository.

## Technical prerequisites

Please bring your own laptop and make sure to install the below items **before** attending the first class:

1. Install `R` from https://cran.r-project.org
2. Install `RStudio Desktop` (Open Source License) from https://www.rstudio.com/products/rstudio/download
3. You probably already have a git and GitHub account, but if not install `git` from https://git-scm.com/ and register an account at https://github.com
4. Open RStudion and enter the following commands in the R console (bottom left panel of RStudio) and make sure you see a plot in the bottom right panel and no error messages in the R console:

```r
install.packages('tidyverse')
library(ggplot2)
ggplot(diamonds, aes(cut)) + geom_bar()
```
5. Bookmark, watch or star this repository so that you can easily find it later.

## Use of version control during the course

- During the course it is required to use your GitHub account. 
- Assignments must be uploaded to your GitHub repo, which needs to be shared with @regulyagoston.
- You may use Shell/Terminal, GitHub Desktop (or other application), or can use RStudio as well to commit and push/pull to your repo.
  * You may check out the guide for [*Help for GitHub and RStudio*](https://github.com/CEU-Economics-and-Business/ECBS-5208-Coding-1-Business-Analytics/blob/main/Help/help_github_n_Rstudio.md).

## RMarkdown

- RMarkdown is a powerful tool, provided in RStudio, which helps to create reports rigorously, automatize reports, create an Html, etc.
- During this course and Data Analysis 2 some of the assignments will be required as an output from RMarkdown.
- However, in many cases it is not easy to get started with RMarkdown and *knit* your first document especially in *pdf* format as it is likely to give an error.
  * You may check out the guidance on [*Help to knit my document in RMarkdown*](https://github.com/CEU-Economics-and-Business/ECBS-5208-Coding-1-Business-Analytics/blob/master/Help/help_rmarkdown.md).

If in doubt feel free to contact me via email: reguly_agoston@phd.ceu.edu.
