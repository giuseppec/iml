# Copied from internal partykit function
list.rules.party <- function(x, i = NULL, ...) {
  if (is.null(i)) {
    i <- partykit::nodeids(x, terminal = TRUE)
  }
  if (length(i) > 1) {
    ret <- sapply(i, list.rules.party, x = x)
    names(ret) <- if (is.character(i)) {
      i
    } else {
      names(x)[i]
    }
    return(ret)
  }
  if (is.character(i) && !is.null(names(x))) {
    i <- which(names(x) %in% i)
  }
  stopifnot(length(i) == 1 & is.numeric(i))
  stopifnot(i <= length(x) & i >= 1)
  i <- as.integer(i)
  dat <- partykit::data_party(x, i)
  if (!is.null(x$fitted)) {
    findx <- which("(fitted)" == names(dat))[1]
    fit <- dat[, findx:ncol(dat), drop = FALSE]
    dat <- dat[, -(findx:ncol(dat)), drop = FALSE]
    if (ncol(dat) == 0) {
      dat <- x$data
    }
  }
  else {
    fit <- NULL
    dat <- x$data
  }
  rule <- c()
  recFun <- function(node) {
    if (partykit::id_node(node) == i) {
      return(NULL)
    }
    kid <- sapply(partykit::kids_node(node), partykit::id_node)
    whichkid <- max(which(kid <= i))
    split <- partykit::split_node(node)
    ivar <- partykit::varid_split(split)
    svar <- names(dat)[ivar]
    index <- partykit::index_split(split)
    if (is.factor(dat[, svar])) {
      if (is.null(index)) {
        index <- ((1:nlevels(dat[, svar])) > partykit::breaks_split(split)) +
          1
      }
      slevels <- levels(dat[, svar])[index == whichkid]
      srule <- paste(svar, " %in% c(\"", paste(slevels,
        collapse = "\", \"", sep = ""
      ), "\")", sep = "")
    }
    else {
      if (is.null(index)) {
        index <- 1:length(kid)
      }
      breaks <- cbind(c(-Inf, partykit::breaks_split(split)), c(
        partykit::breaks_split(split),
        Inf
      ))
      sbreak <- breaks[index == whichkid, ]
      right <- partykit::right_split(split)
      srule <- c()
      if (is.finite(sbreak[1])) {
        srule <- c(srule, paste(svar, ifelse(right, ">",
          ">="
        ), sbreak[1]))
      }
      if (is.finite(sbreak[2])) {
        srule <- c(srule, paste(svar, ifelse(right, "<=",
          "<"
        ), sbreak[2]))
      }
      srule <- paste(srule, collapse = " & ")
    }
    rule <<- c(rule, srule)
    return(recFun(node[[whichkid]]))
  }
  node <- recFun(partykit::node_party(x))
  paste(rule, collapse = " & ")
}
