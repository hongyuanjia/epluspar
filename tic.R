# add Makevars
get_stage("before_install") %>%
    add_code_step(dir.create("/home/travis/.R")) %>%
    add_code_step(writeLines(
        "
        CXX14 = g++-7 -fPIC -flto=2
        CXX14FLAGS = -mtune=native -march=native -Wno-unused-variable -Wno-unused-function -Wno-unused-local-typedefs -Wno-ignored-attributes -Wno-deprecated-declarations -Wno-attributes -O3
        ",
        con = "/home/travis/.R/Makevars"
    ))

# install EnergyPlus v8.8
get_stage("script") %>% 
    add_code_step(eplusr::install_eplus(8.8))

# R CMD Check
args <- "--as-cran"
build_args <- c("--force")
do_package_checks(args = args, build_args = build_args)

# pkgdown
# make sure to clean site to rebuild everything
if (ci_get_branch() == "master" && Sys.getenv("TRAVIS_OS_NAME") == "linux" && Sys.getenv("TRAVIS_R_VERSION_STRING") == "release") {
    do_pkgdown(document = TRUE, orphan = TRUE)
}

# codecov
get_stage("deploy") %>%
  add_code_step(covr::codecov())
