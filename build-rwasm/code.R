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
# call against PPM with pkgcache 2.2.5 / pkgdepends 0.9.1 (and current devel).
# Workaround: skip the broken resolver entirely. We build `package_info`
# ourselves and hand pre-packaged source tarballs to `rwasm:::update_repo(
# remotes = NULL, ...)`, which never enters pkgdepends.

install.packages("remotes")
remotes::install_github("r-wasm/rwasm", upgrade = "never")

# Force rwasm's .onLoad to fire so `options("rwasm.webr_version")` (and
# friends) are populated -- otherwise the R version below resolves to "."
# and every r-wasm.org lookup fails.
loadNamespace("rwasm")

cran_mirror <- "https://packagemanager.posit.co/cran/latest"
avail <- available.packages(
  contriburl = contrib.url(cran_mirror, type = "source")
)

# Pre-fetch r-wasm.org's binary PACKAGES so we know which transitive deps are
# already available there (no need to build them here).
r_ver <- R_system_version(getOption("rwasm.webr_version"))
r_minor <- paste0(r_ver$major, ".", r_ver$minor)
wasm_contrib_url <- sprintf(
  "https://repo.r-wasm.org/bin/emscripten/contrib/%s", r_minor
)
wasm_avail <- utils::available.packages(contriburl = wasm_contrib_url)

repo_abs <- fs::path_abs(repo_path)
contrib_src <- fs::path(repo_abs, "src", "contrib")
fs::dir_create(contrib_src)

base_pkgs <- rownames(installed.packages(priority = "base"))

# ---------------------------------------------------------------------------
# Recursive dependency resolution.
# `packages` lists only refs we *must* build ourselves (typically: blockr.*
# + a few cynkra GitHub-only packages whose CRAN/r-wasm.org version is
# stale). For each ref we clone (GH) or look up (CRAN), read DESCRIPTION,
# and walk Imports/Depends/LinkingTo:
#   - dep declared in some Remotes field of this or an ancestor ref
#     -> resolve as that GH ref, build from source, recurse.
#   - dep already in r-wasm.org binary repo
#     -> nothing to do (consumer will install from r-wasm at runtime).
#   - neither
#     -> abort. User must add the ref to `packages`.
# ---------------------------------------------------------------------------

parse_dep_field <- function(desc, field) {
  if (!field %in% colnames(desc)) return(character())
  raw <- desc[1, field]
  if (is.na(raw) || !nzchar(raw)) return(character())
  parts <- strsplit(raw, "[,\n]")[[1]]
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  # Strip version constraints like "pkg (>= 1.0)"
  sub("[[:space:]]*\\(.*$", "", parts)
}

# A Remotes entry looks like "user/repo", "user/repo@branch",
# "github::user/repo", "bioc::pkg", "url::https://..." etc. We only handle
# the (overwhelmingly common) GitHub form here; anything else is reported.
parse_remotes <- function(desc) {
  entries <- parse_dep_field(desc, "Remotes")
  if (!length(entries)) return(setNames(character(), character()))

  bare <- sub("^[a-zA-Z]+::", "", entries)
  is_gh <- grepl("^[^/]+/[^/]+", bare)
  if (any(!is_gh)) {
    warning(
      "Ignoring non-GitHub Remotes entries: ",
      paste(entries[!is_gh], collapse = ", ")
    )
    bare <- bare[is_gh]
  }

  # Package name = repo (after `/`, before any `@ref`)
  repo <- sub("@.*$", "", bare)
  pkg <- sub("^.*/", "", repo)
  setNames(bare, pkg)
}

# Clone a GH ref into a temp dir, return its DESCRIPTION + cleanup info.
clone_github <- function(ref) {
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
  list(clone_dir = clone_dir, desc = desc, ref = ref)
}

# Re-roll a clone dir as a CRAN-style source tarball, drop it in contrib_src,
# and return the info row that `update_repo()` consumes.
tarball_and_info <- function(clone, desc) {
  pkg <- unname(desc[1, "Package"])
  ver <- unname(desc[1, "Version"])
  clone_dir <- clone$clone_dir

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
    ref = clone$ref,
    status = "OK",
    stringsAsFactors = FALSE
  )
}

# Build info row for a CRAN package (no clone, source URL points at PPM).
cran_info <- function(ref) {
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

visited <- character()
to_build <- list()

resolve <- function(ref, ancestor_remotes = setNames(character(), character())) {
  is_gh <- grepl("/", ref, fixed = TRUE)
  if (is_gh) {
    message("  resolve(GH): ", ref)
    clone <- clone_github(ref)
    desc <- clone$desc
    pkg <- unname(desc[1, "Package"])

    if (pkg %in% visited) {
      unlink(clone$clone_dir, recursive = TRUE)
      return()
    }
    visited <<- c(visited, pkg)

    to_build[[pkg]] <<- tarball_and_info(clone, desc)
  } else {
    message("  resolve(CRAN): ", ref)
    pkg <- ref
    if (pkg %in% visited) return()
    visited <<- c(visited, pkg)
    to_build[[pkg]] <<- cran_info(ref)
    # CRAN refs aren't recursed (assumed to be in r-wasm.org).
    return()
  }

  # Merge this pkg's Remotes with ancestor's (this pkg's wins on conflict).
  remotes_here <- parse_remotes(desc)
  remotes <- c(
    remotes_here,
    ancestor_remotes[setdiff(names(ancestor_remotes), names(remotes_here))]
  )

  # Walk Imports / Depends / LinkingTo
  deps <- unique(c(
    parse_dep_field(desc, "Imports"),
    parse_dep_field(desc, "Depends"),
    parse_dep_field(desc, "LinkingTo")
  ))
  deps <- setdiff(deps, c("R", base_pkgs))

  for (dep_name in deps) {
    if (dep_name %in% visited) next

    if (dep_name %in% names(remotes)) {
      resolve(remotes[[dep_name]], ancestor_remotes = remotes)
    } else if (dep_name %in% rownames(wasm_avail)) {
      # r-wasm.org has it -- consumer pulls at runtime. Nothing to build.
      next
    } else {
      stop(sprintf(
        "Dep '%s' (needed by '%s') is neither in any Remotes nor on %s. ",
        dep_name, pkg, wasm_contrib_url
      ), "Add it explicitly to ./packages.")
    }
  }
}

message("\nResolving package refs (recursive via DESCRIPTION + Remotes)")
for (ref in packages) resolve(ref)

package_info <- do.call(rbind, unname(to_build))

message("\nBuilding wasm binaries for ", nrow(package_info), " package(s):")
message(paste("  -", package_info$package, collapse = "\n"))

# `remotes = NULL` makes `prefer_remotes()` a no-op so we don't accidentally
# re-enter pkgdepends through the webr-remotes resolution path.
rwasm:::update_repo(
  package_info,
  remotes = NULL,
  repo_dir = repo_abs,
  compress = compress_lgl
)

# `update_repo()` only refreshes PACKAGES when it freshly downloaded a source
# tarball via `make_remote_tarball()`. We pre-place tarballs in `contrib_src`
# ourselves, so its `need_update` flag stays FALSE and PACKAGES is never
# written -- 404'ing consumers. Force the refresh here.
contrib_bin <- fs::path(repo_abs, "bin", "emscripten", "contrib", r_minor)
tools::write_PACKAGES(contrib_src)
tools::write_PACKAGES(contrib_bin, type = "mac.binary")
message("Refreshed PACKAGES indexes in src/ and bin/emscripten/contrib/", r_minor)
