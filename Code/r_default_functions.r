get_os <- function() {
  if (.Platform$OS.type == "windows") {
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "mac"
  } else if (.Platform$OS.type == "unix") {
    "unix"
  } else {
    stop("Unknown OS")
  }
}

install_lazy <- function(pkg_list, verbose=TRUE) {
  installed_packages <- installed.packages()[, 1]
  need_to_install <- setdiff(pkg_list, installed_packages)
  already_installed <- pkg_list[pkg_list %in% installed_packages]
  for (pkg in need_to_install) {
    try(install.packages(pkg), silent=TRUE)
  }
  if (verbose) {
    message("Already installed:")
    print(already_installed)
    newly_installed <- need_to_install[need_to_install %in% installed.packages()]
    if (length(newly_installed) > 0) {
      message("Newly installed:")
      print(newly_installed)
    }
  }
  failed_to_install <- setdiff(need_to_install, installed.packages())
  if (length(failed_to_install) > 0) {
    warning("Failed to install these packages:\n  ", paste(failed_to_install))
  }
}

clear_all <- function() {
  # clear and close any open grapics devices, then delete everything.
  while (! is.null(dev.list())) {
    while(dev.flush() > 0) {
      # do nothing.
    }
    try(dev.off(), silent = TRUE)
  }
  rm(list = ls(envir = .GlobalEnv, all.names = TRUE, sorted = FALSE),
     envir = .GlobalEnv)
}
