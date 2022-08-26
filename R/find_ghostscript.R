system.partition = 'c:'
dirs = c('Program Files', 'Program Files (x86)')
for (dir in dirs) {
  dir.list = list.dirs(file.path(system.partition, dir), recursive = FALSE)
  GsinList = grepl(pattern = 'gs', x = dir.list)
  if (sum(GsinList) > 0) {
    gsDirectory = which(GsinList == TRUE)
    GsExeFiles = list.files(
      dir.list[gsDirectory],
      recursive = TRUE,
      pattern = 'gswin(.*)c.exe',
      include.dirs = TRUE,
      full.names = TRUE
    )[1]
    message('Gs found! ~> ',GsExeFiles)
    Sys.setenv(R_GSCMD = GsExeFiles)
    break
  }
}

# Sys.setenv(R_GSCMD = r'(c:/Program Files/gs/gs9.52/bin/gswin64c.exe)')
