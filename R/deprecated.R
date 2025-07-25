.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n",
    "Some changes to functions in the current version of ", pkgname," may require\n",
    "updates of existing code.\n\n",
    "To see which functions have changed, please see\n",
    "https://cran.r-project.org/web/packages/Rmonize/news/news.html"
  )
}
