if (base::file.exists("renv.lock")) {
  base::stop("renv appears to be set up. Just run `renv::restore() to begin.`")
}

base::options(repos = c(CRAN = "https://cran.rstudio.com"))
# Bootstrap load pacman
if (!base::require("pacman")) utils::install.packages("pacman")
# Load renv
pacman::p_load_gh("rstudio/renv")

renv::init()

# Work around bug in {remotes} with having {renv} .Rprofile
base::cat(
  "# Conditionally load to get around a bug in {remotes}
if (base::file.exists(\"renv/activate.R\")) {
  base::source(\"renv/activate.R\")
}
",
  file = "./.Rprofile"
)

# Try installing this first, since `renv::update()` wasn"t working.
renv::install("vctrs")
renv::update()
renv::upgrade()
renv::snapshot()
