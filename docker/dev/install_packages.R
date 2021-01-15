# installing package imports packages
pkg_list <- c("dplyr",
              "lubridate",
              "magrittr",
              "rmarkdown",
              "flexdashboard",
              "plotly",
              "reactable")

install.packages(pkgs = pkg_list, repos = "https://cran.rstudio.com/")

fail <- FALSE

for(i in pkg_list){
  
  if(i %in% rownames(installed.packages())){
    cat(i, "...OK\n")
  } else {
    cat(i, "...Fail\n")
    fail <- TRUE
  }
  
  if(fail){
    stop("Fail to install some package/s")
  }
}