is_windows <- function(){
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

java_path_list <- function(){
  fs::dir_ls(fs::path(jabba_loc(),"jdk"))
}
