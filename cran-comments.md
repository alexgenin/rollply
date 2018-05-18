
# Release Summary

This updated version of rollply brings the following improvements :
  * documentation is greatly improved (incorrect documentation was generated
      in rollply 0.4.2).
  * a vignette is now included in the package ( vignette('rollply') )
  * some issues were fixed regarding the creation of grids
  * an internal implementation of alphahull::inahull is now used to provide
      much quicker generation of alphahull-based grids.

# Test environments and results

* Local install (R 3.2.3 on Arch Linux as of 2016-03-23)
  No error or warnings (R CMD check status: OK).

* win-builder (R-devel and R-release as of 2016-03-24)
  No error or warnings (R CMD check status: OK).
