#' @name LDA
#' @alias LDA
#' @title Latent Dirichlet Allocation (LDA)
#' @description Estimate a LDA model using either a VEN algorithm or Gibbs sampling.
#' @usage (x, k, method = "VEM", control = NULL, model = NULL, ...)
#' @param x Any object coercible to such simple triplet matrices (with count entries),
#' @param k Integer; number of topics
#' @param method The method to be used for fitting the model; either "VEM" or "Gibbs".
#' @param control A named listed of the control parameters for estimation
#' @param model Object of class "LDA" for initialization
#' @param ... Optional arguments. For method = "Gibbs" an additional argument seedwords can be specified as a matrix or an object of class "simple_triplet_matric".
#' @examples
#' LDA(x, k, method = "VEM", control = NULL, model = NULL)
#' @keywords LDA
#' @export

match_terms <- function(x, model) {
  if (!(is.null(model@terms) | is.null(colnames(x)))) {
    x <- x[,which(colnames(x) %in% model@terms)]
    x <- x[,order(match(colnames(x), model@terms))]
    js <- match(colnames(x), model@terms)
    x$j <- js[x$j]
    x$ncol <- model@Dim[2]
    dimnames(x)[[2]] <- model@terms
  } else if (ncol(x) != model@Dim[2]) stop("The number of terms in the input matrix and the fitted model need to match")
  x
}

LDA_registry <- list(LDA_VEM.fit = c("VEM", "LDA_VEM", "LDA_VEM.fit"),
                     LDA_Gibbs.fit = c("Gibbs", "LDA_Gibbs", "LDA_Gibbs.fit"))

LDA <- function(x, k, method = "VEM", control = NULL, model = NULL, ...)
{
  if (is(x, "DocumentTermMatrix")) {
    if (!any(attr(x, "weighting") %in% c("term frequency", "tf"))) {
      stop("The DocumentTermMatrix needs to have a term frequency weighting")
    }
  } else if (!is(x, "simple_triplet_matrix")) {
    x <- as.simple_triplet_matrix(x)
  }
  if (!all.equal(x$v, as.integer(x$v)))
    stop("Input matrix needs to contain integer entries")
  if (!all(slam::row_sums(x) > 0))
    stop("Each row of the input matrix needs to contain at least one non-zero entry")
  mycall <- match.call()

  if (!is.null(model)) {
    x <- match_terms(x, model)
    k <- model@k
  }

  if (as.integer(k) != k || as.integer(k) < 2) stop("'k' needs to be an integer of at least 2")

  if(missing(method) && !missing(model))
    method <- paste(class(model), "fit", sep = ".")
  if(!is.function(method)) {
    MATCH <- which(sapply(LDA_registry, function(x) length(grep(tolower(method), tolower(x)))) > 0)
    if (!length(MATCH) == 1)
      stop("'method' not specified correctly")
    method <- get(names(LDA_registry)[MATCH])
  }

  method(x, k, control, model, mycall, ...)
}

LDA_VEM.fit <- function(x, k, control = NULL, model = NULL, call, ...) {
  control <- as(control, "LDA_VEMcontrol")
  if (length(control@seed) != control@nstart)
    stop(paste("Need ", control@nstart, " seeds", sep = ""))
  if (length(control@alpha) == 0)  {
    control@alpha <- if (!is.null(model)) model@alpha else 50/k
  }
  if (is.null(model)) {
    if (control@initialize == "model")
      stop("Need a model of class 'LDA_VEM' for initialization")
  }
  else control@initialize <- "model"
  if (!control@estimate.beta) control@em@iter.max <- -1L
  result_dir <- path.expand(paste(control@prefix, "-lda", sep = ""))

  if (control@save) dir.create(result_dir, showWarnings = FALSE)

  obj <- vector("list", control@nstart)
  for (i in seq_len(control@nstart)) {
    control_i <- control
    control_i@seed <- control@seed[i]
    obj[[i]] <- .Call(C_rlda,
                      ## simple_triplet_matrix
                      as.integer(x$i),
                      as.integer(x$j),
                      as.numeric(x$v),
                      as.integer(x$nrow),
                      as.integer(x$ncol),
                      ## LDAcontrol
                      control_i,
                      ## number of topics
                      as.integer(k),
                      ## directory for output files
                      result_dir,
                      ## initial model
                      model)
    obj[[i]]@gamma <- obj[[i]]@gamma/rowSums(obj[[i]]@gamma)
    obj[[i]] <- new(class(obj[[i]]), obj[[i]], call = call, control = control_i,
                    documents = x$dimnames[[1]], terms = x$dimnames[[2]], n = as.integer(sum(x$v)))
  }
  if (control@best) {
    MAX <- which.max(sapply(obj, logLik))
    if (length(MAX)) {
      obj <- obj[[MAX]]
    } else warning("problem selecting best fitting model")
  }
  obj
}

LDA_Gibbs.fit <- function(x, k, control = NULL, model = NULL, call, seedwords = NULL, ...) {
  if (!is.null(model) && is(control, "list") && !"delta" %in% names(control)) control <- c(control, delta = model@control@delta)
  if (!is.null(model) && is.null(seedwords)) seedwords <- model@seedwords
  if (!is.null(model) && is(control, "list") && !"initialize" %in% names(control)) control <- c(control, initialize = "beta")
  control <- as(control, "LDA_Gibbscontrol")
  if (length(control@seed) != control@nstart)
    stop(paste("Need ", control@nstart, " seeds", sep = ""))
  if (length(control@alpha) == 0)  {
    control@alpha <- if (!is.null(model)) model@alpha else 50/k
  }

  if(control@initialize == "z") {
    if (k != max(model@z)) {
      k <- max(model@z)
      warning("'k' set to ", k)
    }
    if (sum(x$v) != length(model@z))
      stop("Dimension of data and fitted model need to match for initialization")
  }
  if(control@initialize == "beta") {
    if (k != nrow(model@beta)) {
      k <- nrow(model@beta)
      warning("'k' set to ", k)
    }
    if (ncol(x) != ncol(model@beta))
      stop("Dimension of data and fitted model need to match for initialization")
  }

  result_dir <- path.expand(paste(control@prefix, "-lda", sep = ""))
  if (control@save) dir.create(result_dir, showWarnings = FALSE)
  seedwords_matrix <- if (is.null(seedwords)) NULL else control@delta + as.matrix(seedwords)
  if (!is.null(seedwords)) seedwords <- slam::as.simple_triplet_matrix(seedwords)
  CONTROL_i <- control
  CONTROL_i@iter <- control@burnin + control@thin
  obj <- vector("list", control@nstart)
  for (i in seq_len(control@nstart)) {
    if (!is.na(CONTROL_i@seed[i])) set.seed(CONTROL_i@seed[i])
    obj[[i]] <- list(.Call(C_rGibbslda,
                           ## simple_triplet_matrix
                           as.integer(x$i),
                           as.integer(x$j),
                           as.integer(x$v),
                           as.integer(x$nrow),
                           as.integer(x$ncol),
                           ## LDAcontrol
                           CONTROL_i,
                           ## initialize
                           switch(control@initialize, beta = 1L, z = 2L, 0L),
                           !is.null(seedwords),
                           seedwords_matrix,
                           ## number of topics
                           as.integer(k),
                           ## directory for output files
                           result_dir,
                           ## initial model
                           if (is.null(model)) NULL else model@beta,
                           if (is.null(model)) NULL else model@z))
    obj[[i]][[1]] <- new(class(obj[[i]][[1]]), obj[[i]][[1]], call = call, control = CONTROL_i, seedwords = seedwords,
                         documents = x$dimnames[[1]], terms = x$dimnames[[2]], n = as.integer(sum(x$v)))
    iterations <- unique(c(seq(CONTROL_i@iter, control@burnin + control@iter, by = control@thin),
                           control@burnin + control@iter))
    if (length(iterations) > 1) {
      for (j in seq_along(iterations)[-1]) {
        CONTROL_i@iter <- diff(iterations)[j-1]
        obj[[i]][[j]] <- .Call(C_rGibbslda,
                               ## simple_triplet_matrix
                               as.integer(x$i),
                               as.integer(x$j),
                               as.integer(x$v),
                               as.integer(x$nrow),
                               as.integer(x$ncol),
                               ## LDAcontrol
                               CONTROL_i,
                               ## initialize
                               2L,
                               !is.null(seedwords),
                               seedwords_matrix,
                               ## number of topics
                               as.integer(k),
                               ## directory for output files
                               result_dir,
                               ## initial model
                               obj[[i]][[j-1]]@beta,
                               obj[[i]][[j-1]]@z)
        obj[[i]][[j]] <- new(class(obj[[i]][[j]]), obj[[i]][[j]], call = call, control = CONTROL_i, seedwords = seedwords,
                             documents = x$dimnames[[1]], terms = x$dimnames[[2]], n = as.integer(sum(x$v)))
      }
      if (control@best) obj[[i]] <- obj[[i]][[which.max(sapply(obj[[i]], logLik))]]
    } else if (control@best) obj[[i]] <- obj[[i]][[1]]
  }
  if (control@best) {
    MAX <- which.max(sapply(obj, logLik))
    if (length(MAX)) {
      obj <- obj[[MAX]]
    } else warning("no finite likelihood")
  } else {
    obj <- lapply(obj, function(x) new("Gibbs_list", fitted = x))
    if (control@nstart == 1) obj <- obj[[1]]
  }
  obj
}


