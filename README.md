## sta380_repo
Repository for 2026 STA380H5 final project

Team name: Chou Why did

Authors: Renfei Wu, Shiqi Tian, Chengxi Li, Liwen Yin

## Initialization

Clone this repo to your machine. In RStudio:  
File -> New Project -> Existing Directory -> select this repo.

## Requirement

Run the following to get `devtools` for your R
```r
install.packages("devtools")
```

The following package is also required:
```r
install.packages(c("testthat", "roxygen2", "rmarkdown", "knitr"))
```

Finally, run the following to check your environment is ready or not
```r
devtools::check()
```

### Documents

To render R Code's documentation, run in your terminal:
```bash
devtools::document()
```
`.Rd` file should be compiled in the `man/` folder

To render the vignette to PDF, Run:
```r
devtools::build_vignettes()
```
pdf should be compiled in the `doc/` folder.


## Testing
  
In your R console:
```r
devtools::test()
```
