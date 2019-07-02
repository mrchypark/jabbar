is_window <- function(){
  Sys.info()$sysname == "Windows"
}
is_mac <- function(){
  Sys.info()$sysname == "Darwin"
}
is_linux <- function(){
  Sys.info()$sysname == "Linux"
}

#' @rdname jabba-tools
#' @export
jabba_binary <- function(jabba = "auto") {
  
  # automatic lookup if requested
  if (identical(jabba, "auto")) {
    jabba <- find_jabba()
    if (is.null(jabba))
      stop("Unable to find jabba binary. Please install_jabba first.", call. = FALSE)
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
}

get_jabba <- function(){
  
}



jabba_()

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
