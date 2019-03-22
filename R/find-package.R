
# attempt to find a package 'package' from source 'source' in a set of
# library paths
renv_package_find <- function(package,
                              source = "CRAN",
                              libpaths = renv_libpaths_all())
{
  # first, look in the library paths (specifiy lib.loc so that we avoid
  # using the path to loaded namespaces)
  location <- find.package(package, quiet = TRUE, lib.loc = libpaths) %||% ""
  if (renv_file_exists(location))
    return(location)

  # if that failed, try looking in the cache for the latest
  # version of the package
  location <- renv_paths_cache(package)
  versions <- list.files(location)
  if (!length(versions))
    return("")

  # take the most recent version
  sorted <- versions[order(numeric_version(versions))]
  version <- tail(sorted, n = 1)

  # now, check and see how many installations are associated with
  # this version
  hashes <- list.files(file.path(location, version))
  if (length(hashes) == 0)
    return("")

  # figure out the sources for these packages
  paths <- file.path(location, version, hashes, package)
  descs <- lapply(paths, renv_description_read)
  types <- map_chr(descs, renv_snapshot_description_source)

  # keep the entries that match the requested source
  keep <- paths[tolower(types) == tolower(source)]
  if (empty(keep))
    return("")

  # TODO: should we notify user if multiple versions of this package were found?
  keep[[1]]

}
