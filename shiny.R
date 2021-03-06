library(shiny)
if (!"ggrepel" %in% installed.packages()) {
    install.packages(c("ggrepel"), repo = "https://cloud.r-project.org")
}
#library(Cairo)
library_path <- paste("Library Path: ", Sys.getenv(c("LD_LIBRARY_PATH")))
print(paste("LD_LIBRARY_PATH: ", library_path))

lib_dir <- '/home/vcap/deps/0/r/lib'
local_lib_dir <- 'lib'

if(dir.exists(lib_dir))
{
  # Get the list of libs
  lib_tars <- list.files(local_lib_dir)
  lib_tars <- paste(local_lib_dir, lib_tars, sep="/")

  print(paste("Local libs: ", lib_tars))
  print(paste("Working directory: ", list.files(getwd())))

  # Copy the files to the lib_dir
  for(i in 1:length(lib_tars)) {untar(lib_tars[i], exdir = lib_dir)}

  Sys.setenv(PROJ_LIB=lib_dir)
}

print(list.files(lib_dir))
#options(device=Cairo())
runApp(host="0.0.0.0", port=strtoi(Sys.getenv("PORT")))