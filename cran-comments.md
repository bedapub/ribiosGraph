## Test environments

* Local: Linux Mint 21.2 (x86_64), R 4.5.2
* GitHub Actions:
  - Windows (R release)
  - macOS (R release, R devel)
  - Ubuntu (R release, R oldrel)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes for CRAN reviewers

This package provides tools to manipulate and visualize graphs (networks)
for computational biology in drug discovery, developed at F. Hoffmann-La
Roche AG.

Key functionality includes:
- Creating bipartite graphs from data frames
- Interactive network visualizations using plotly

The package depends on ribiosUtils (on CRAN) and igraph.
