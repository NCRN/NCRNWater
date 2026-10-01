# zzz.R
#
# Console message on library()
#
.onAttach <- function(libname, pkgname) {
  pkg_version <- utils::packageDescription(pkgname, fields = "Version")
  
  packageStartupMessage(
    # sprintf("This is %s %s", pkgname, pkg_version)
    sprintf("🛠 %s %s loaded", pkgname, pkg_version)
  )
}
