# cwainright 2025-05-09
# to reproduce the dev environment that this app was built in:
install.packages('renv')
renv::activate() # tells your R project to use renv to manage packages locally instead of gloablly via install.packages()
renv::restore() # tells your R project to install all of the packages from the renv.lock file


# alternatively, how to rebuild the environment
renv::install(
    c(
        'shiny'
        ,'lattice'
        ,'dplyr'
        ,'lubridate'
        ,'DT'
        ,'htmltools'
        ,'ggplot2'
        ,'leaflet'
        ,'jsonlite'
        ,'purrr'
        ,'magrittr'
        ,'openair'
        ,'NADA'
        ,'plotly'
        ,'gh'
        ,'ini'
        ,'miniUI'
        ,'munsell'
        ,'sp'
        ,'textshaping'
        ,'xtable'
        ,'Rcpp'
        ,'gert'
        ,'pkgbuild'
        ,'rcmdcheck'
        ,'urlchecker'
        ,'usethis'
        ,'zip'
        ,'httpuv'
        ,'promises'
        ,'tidyr'
        ,'bslib'
        ,'rstudioapi'
        ,'callr'
        ,'mapproj'
        ,'remotes'
    )
    ,rebuild = T
    , prompt = F
)

options(download.file.method = "wininet")
remotes::install_github('https://github.com/ncrn/ncrnwater')
