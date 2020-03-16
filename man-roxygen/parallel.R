#' @section Parallelization:
#' Parallelization is supported via package \CRANpkg{future}.
#' To initialize future-based parallelization, select an appropriate backend and
#' specify the amount of workers.
#' For example, to use a PSOCK based cluster backend do:
#' 
#' ```r
#' future::plan(multisession, workers = 2)
#' <iml function here>
#' ```
#' 
#' Consult the resources of the \CRANpkg{future} package for more parallel
#' backend options.
