is_window <- function(){
  as.logical(Sys.info()["sysname"] == "Windows")
}

is_mac <- function(){
  as.logical(Sys.info()["sysname"] == "Darwin")
}

is_linux <- function(){
  as.logical(Sys.info()["sysname"] == "Linux")
}

os_what <- function(){
  switch(
    Sys.info()["sysname"],
    Windows = "windows",
    Darwin = "darwin",
    Linux = "linux"
  )
}


#' @rdname jabba-tools
#' @export
jabba_binary <- function(jabba = "auto") {
  
  # automatic lookup if requested
  if (identical(jabba, "auto")) {
    jabba <- find_jabba()
    if (is.null(jabba))
      stop("Unable to find jabba binary. Please download_jabba() first.", call. = FALSE)
    jabba <- jabba[[1]]
  }
  
  # validate existence
  if (!file.exists(jabba))
    stop("Specified jabba binary '", jabba, "' does not exist.", call. = FALSE)
  
  # return jabba
  jabba
}

jabba_version <- function(jabba = "auto") {
  jabba_bin <- jabba_binary(jabba)
  system2(jabba_bin, "--version", stdout = TRUE)
}

find_jabba <- function(){
  jabba <- Sys.which("jabba")
  if (!nzchar(jabba)) {
    jabba_locations  <- c(
      path.expand("~/jabba/")
    )
    if (is_windows()) {
      jabba_scripts <- c(
        path.expand("~/jabba/")
      )
    }
    jabba_locations <- jabba_locations[file.exists(jabba_locations)]
    if (length(jabba_locations) > 0)
      jabba_locations
    else
      NULL
  } else {
    jabba
  }
}

get_jabba <- function(){
  tar_urls <- jabba_release()
  os <- os_what()
  tar_url <- grep(os, tar_urls, value = T)
  if (is_window()) {
    download.file(tar_url, destfile = , mode = "wb")
  }
}

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

jabba_loc <- function(){
  
}
