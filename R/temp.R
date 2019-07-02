jabba_version_list <- function() {
  httr::GET("https://api.github.com/repos/shyiko/jabba/releases") %>% 
    httr::content() %>% 
    sapply(function(x) x$body) %>% 
    trimws()
}

jabba_release <- function(version = "latest"){
  if (version == "latest"){
    tar <- paste0("https://api.github.com/repos/shyiko/jabba/releases/", version)
  } else {
    tar <- paste0("https://api.github.com/repos/shyiko/jabba/releases/tags/", version)
  }
  tar %>% 
    httr::GET() %>% 
    httr::content() %>% 
    with(assets) %>% 
    sapply(function(x) x$browser_download_url)
}
