geo <- "County_name" 

rmarkdown::render(
  input = "weekly_county_specific.Rmd", 
  params = list(geography = geo)
)

fs::file_copy("weekly_county_specific.html", 
              glue::glue("weekly_county_{geo}.html"), overwrite = TRUE)
