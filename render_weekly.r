
render_weekly <- function(){
  
last_update <- readRDS("last_update.rds") # previously saved when running the 

if (identical(last_update, file.info("./deathfile.csv")$mtime)) {
  stop(stringr::str_glue("There is no change since the last update on {last_update})"), call. = F)
}

rmarkdown::render("weekly_reports.Rmd")

}

render_weekly()

