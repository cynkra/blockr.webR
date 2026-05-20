args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("No args supplied to Rscript. ")
}

image_path <- args[1]
repo_path <- args[2]
compress <- args[3]

if (!nzchar(image_path) && !nzchar(repo_path)) {
  stop("At least one of `image-path` or `repo-path` should be `true`.")
}

gha_dir <- file.path("/github/workspace")

packages <- args[4]
strip <- args[5]

packages <- strsplit(packages, "[[:space:],]+")[[1]]
strip <- strsplit(strip, "[[:space:],]+")[[1]]
if (is.character(strip) && length(strip) == 1 && strip == "NULL") strip <- NULL

compress_lgl <- isTRUE(as.logical(compress))

cat("\nArgs:\n")
str(list(
  image_path = image_path,
  repo_path = repo_path,
  packages = packages,
  strip = strip,
  compress = compress_lgl
))

if (!require("withr", character.only = TRUE, quietly = TRUE)) {
  install.packages("withr")
}

# Work in the GHA directory so that package reference 'local::.' works as expected
withr::local_dir(gha_dir)

# If GITHUB_PAT isn't found, use GITHUB_TOKEN
withr::local_envvar(list(
  "GITHUB_PAT" = Sys.getenv("GITHUB_PAT", Sys.getenv("GITHUB_TOKEN"))
))

# `rwasm::add_pkg()` goes through `pkgdepends::new_pkg_download_proposal()$resolve()`,
# which hits `Error in res_one_row_df(entries): nrow(out) must equal 1` on every
# call against PPM with pkgcache 2.2.5 / pkgdepends 0.9.1 (and current devel
# 2.2.5.9000 / 0.9.1.9000 — verified by installing both from r-universe; the
# bug still fires). It is unfixed upstream as of writing:
#   https://github.com/r-lib/pak/issues/804
#   https://github.com/r-lib/pkgdepends/issues/462
#   https://github.com/r-wasm/rwasm/issues/56
#   https://github.com/khusmann/irid/issues/6  (working bypass blueprint)
#
# Workaround: skip the broken resolver entirely. Build a `package_info` data
# frame ourselves (CRAN refs via `available.packages()`, GitHub refs via shallow
# `git clone` + DESCRIPTION) and hand pre-packaged source tarballs to
# `rwasm:::update_repo(remotes = NULL, ...)`. That call path never enters
# pkgdepends. Drop this once a fixed pkgcache reaches CRAN.

install.packages("remotes")
remotes::install_github("r-wasm/rwasm", upgrade = "never")

cran_mirror <- "https://packagemanager.posit.co/cran/latest"
avail <- available.packages(
  contriburl = contrib.url(cran_mirror, type = "source")
)

repo_abs <- fs::path_abs(repo_path)
contrib_src <- fs::path(repo_abs, "src", "contrib")
fs::dir_create(contrib_src)

resolve_ref <- function(ref) {
  if (grepl("/", ref, fixed = TRUE)) {
    # GitHub ref: "user/repo" optionally with "@ref"
    parts <- strsplit(ref, "@", fixed = TRUE)[[1]]
    gh_repo <- parts[1]
    git_ref <- if (length(parts) > 1) parts[2] else NULL

    clone_dir <- tempfile("rwasm-clone-")
    pat <- Sys.getenv("GITHUB_PAT")
    url <- if (nzchar(pat)) {
      sprintf("https://x-access-token:%s@github.com/%s.git", pat, gh_repo)
    } else {
      sprintf("https://github.com/%s.git", gh_repo)
    }
    git_args <- c("clone", "--depth=1", "--quiet")
    if (!is.null(git_ref)) git_args <- c(git_args, "--branch", git_ref)
    git_args <- c(git_args, url, clone_dir)
    rc <- system2("git", git_args)
    if (rc != 0) stop(sprintf("git clone failed for '%s'", ref))

    desc <- read.dcf(file.path(clone_dir, "DESCRIPTION"))
    pkg <- unname(desc[1, "Package"])
    ver <- unname(desc[1, "Version"])

    # Re-roll as a standard CRAN-style source tarball and place it directly
    # in `src/contrib/` so `update_repo()` skips its `make_remote_tarball`
    # download step (which would try to hit a non-existent CRAN URL).
    pkg_root <- file.path(dirname(clone_dir), pkg)
    if (file.exists(pkg_root)) unlink(pkg_root, recursive = TRUE)
    file.rename(clone_dir, pkg_root)
    unlink(file.path(pkg_root, ".git"), recursive = TRUE)

    target <- fs::path(contrib_src, sprintf("%s_%s.tar.gz", pkg, ver))
    withr::with_dir(
      dirname(pkg_root),
      utils::tar(target, files = pkg, compression = "gzip", tar = "internal")
    )
    unlink(pkg_root, recursive = TRUE)

    data.frame(
      package = pkg,
      version = ver,
      sources = I(list(paste0("file://", target))),
      target = sprintf("src/contrib/%s_%s.tar.gz", pkg, ver),
      ref = ref,
      status = "OK",
      stringsAsFactors = FALSE
    )
  } else {
    # CRAN ref
    if (!ref %in% rownames(avail)) {
      stop(sprintf("CRAN package '%s' not found on %s", ref, cran_mirror))
    }
    pkg <- ref
    ver <- unname(avail[ref, "Version"])
    repo_url <- unname(avail[ref, "Repository"])
    data.frame(
      package = pkg,
      version = ver,
      sources = I(list(sprintf("%s/%s_%s.tar.gz", repo_url, pkg, ver))),
      target = sprintf("src/contrib/%s_%s.tar.gz", pkg, ver),
      ref = ref,
      status = "OK",
      stringsAsFactors = FALSE
    )
  }
}

message("\n\nResolving package refs (bypassing pkgdepends):")
infos <- lapply(packages, function(ref) {
  message("  - ", ref)
  resolve_ref(ref)
})
package_info <- do.call(rbind, infos)

message("\n\nBuilding wasm binaries:")
# `remotes = NULL` makes `prefer_remotes()` a no-op so we don't accidentally
# re-enter pkgdepends through the webr-remotes resolution path.
rwasm:::update_repo(
  package_info,
  remotes = NULL,
  repo_dir = repo_abs,
  compress = compress_lgl
)

