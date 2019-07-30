jabba <- function(..., jabba = "auto"){
  jabba_bin <- jabba_binary(jabba)
  cat(paste0(system2(jabba_bin, list(...), stdout = TRUE), collapse = "\n"))
}

#' @rdname jabba-tools
#' @export
jabba_binary <- function(jabba = "auto") {
  
  # automatic lookup if requested
  if (identical(jabba, "auto")) {
    jabba <- find_jabba()
    if (is.null(jabba))
      stop("Unable to find jabba binary. Please get_jabba() first.", call. = FALSE)
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
      fs::path(jabba_loc(),"bin","jabba")
    )
    if (is_windows()) {
      jabba_scripts <- c(
        fs::path(jabba_loc(),"bin","jabba",ext = "exe")
      )
      jabba_locations <- c(jabba_locations, jabba_scripts)
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

get_jabba <- function(path = jabba_loc()){
  tar_urls <- jabba_release()
  tar_urls <- grep(tar_urls, "amd64", value = T)
  os <- os_what()
  tar_url <- grep(os, tar_urls, value = T)
  path <- fs::path(path, "bin")
  
  if (!fs::dir_exists(path)){
    fs::dir_create(path)
  }
  
  if (is_windows()) {
    path <- fs::path(path,"jabba",ext = "exe")
    download.file(tar_url, destfile = path, mode = "wb")
  } else {
    path <- fs::path(path,"jabba")
    download.file(tar_url, destfile = path)
  }
}

jabba_loc <- function(){
  fs::path(fs::path_home(),".jabba")
}
