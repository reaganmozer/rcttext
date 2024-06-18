#' Select a random sample of documents
#'
#' Functions to select random samples of documents using different sampling schemes and/or
#' along different design criteria.
#'
#' Select a random sample of documents
#'
#' Functions to select random samples of documents using different
#' sampling schemes and/or along different design criteria.
#'
#' @param x A \link{corpus} object or character vector of text
#'   documents or dataframe.
#' @param size a non-negative integer giving the number of documents
#'   to sample.
#' @param prob a vector of probability weights for each document.
#' @param wt.fn a function for generating probability weights; ignored
#'   when \code{prob} is used. See Details.
#' @param method the following methods are implemented: simple random
#'   sampling without replacement (`srswor`), simple random sampling
#'   with replacement (`srswr`), Poisson sampling (`poisson`),
#'   systematic sampling (`systematic`); if \code{method} is missing,
#'   the default method is \code{srswor}.
#' @param scheme optional sampling scheme to implement.  NOT YET
#'   IMPLEMENTED.
#' @param return.data logical; if \code{TRUE}, the function returns
#'   the subset of x that are sampled. FALSE returns a vector of row
#'   numbers corresponding to the sampled documents.
#' @return Returns either the sampled data or a vector of rownumbers
#'   of sampled documents.
#'
#' @examples
#'
#' # Load example dataframe
#' data("toy_reads")
#'
#' ## Example 1:Sample 4 documents using the default method
#' ##           (simple random sampling without replacement)
#' textamp_df = textsamp(toy_reads, size = 4)
#'
#' ## Example 2: Sample 8 documents using Poisson sampling
#' textamp_df2 = textsamp(toy_reads, size = 8, method = "poisson")
#'
#' ## Example 3: Sample 8 documents using systematic sampling,
#' ##            but only return the row numbers
#' textamp_df3 = textsamp(toy_reads, size = 8,
#'                        method = "systematic", return.data = FALSE)
#' @export

textsamp <- function(x,
                     size = length(x), prob = NULL, wt.fn = NULL, scheme = NULL,
                     method = c( "srswor", "srswr", "systematic", "poisson" ),
                     return.data = TRUE) {
  # Match the specified method with available options
  method <- match.arg(method,
                      choices = c( "srswor", "srswr", "systematic", "poisson" ))

  # Check if the specified sample size is valid
  if( size < 0 || size > nrow(x) ){
    stop("Invalid sample size. Size must be non-negative and less than the number of documents.")
  }

  # Generate probability weights using the provided function if prob is NULL
  if ( !is.null( wt.fn ) && is.null( prob ) ) {
    prob <- wt.fn(x)
  }

  # Simple Random Sampling Without Replacement
  if ( method == "srswor" ) {
    selected_rows <- sample( nrow( x ), size, replace = FALSE, prob = prob )

    # Simple Random Sampling With Replacement
  } else if ( method == "srswr" ) {
    selected_rows <- sample( nrow( x ), size, replace = TRUE, prob = prob )

    # Poisson Sampling
  } else if ( method == "poisson" ) {
    # Generate inclusion probabilities for Poisson sampling
    pik <- sampling::inclusionprobabilities( 1:nrow( x ), size )
    current_sum <- 0
    target_sum <- size
    # Perform Poisson sampling until the desired number of samples is obtained
    while ( current_sum != target_sum ) {
      s <- sampling::UPpoisson( pik )
      current_sum <- sum( s == 1 )
    }
    selected_rows <- which( s == 1 )

    # Systematic Sampling
  } else if ( method == "systematic" ) {
    # Calculate the step size for systematic sampling
    step <- ceiling( nrow( x ) / size )
    # Randomly select a starting point within the step
    start <- sample( 1:step, 1 )
    # Select the row numbers based on the systematic sampling scheme
    selected_rows <- seq( start, by = step, length.out = size )
  }

  # Return either the selected data or the row numbers
  if ( return.data ) {
    return( x[selected_rows, ] )
  } else {
    return( selected_rows )
  }
}

#' @rdname textsamp
#'
#'   Sample `size` documents from corpus x, stratifying the sample by
#'   strata defined by unique combinations of passed stratification
#'   variables. Say the stratification variables divide the documents
#'   into K strata. This method will sample $size_k \propto n_k$
#'   documents from each strata, where $n_k$ is the number of
#'   documents in stratum $k$.  If these numbers do not divide, this
#'   method will sample the closest integer number of documents to the
#'   given proportion.
#'
#'   Proportions will be calculated as round( size * n_k / n ), where
#'   n is the number of documents total.
#'
#' @param by One of two things: either a \code{data.frame} with
#'   document-level stratification variable(s) or a character vector
#'   with the names of the stratification variables to be found in
#'   `docvars(x)`
#' @param size Total number of documents to sample
#' @param equal_size_samples TRUE means sample same number of
#'   documents from each strata.  FALSE means sample proportional to
#'   strata size, as described above.
#' @param ... additional arguments passed on to `textsamp`. Cannot
#'   include `scheme`.
#'
#' @examples
#'
#' # Load example dataframe
#' data("toy_reads")
#'
#' ## Example 1: Stratified Sampling by a Single Variable
#' stratified_df1 <- textsamp_strata(toy_reads, size = 6, by = "more")
#'
#' Example 2: Stratified Sampling by Multiple Variables
#' stratified_df2 <- textsamp_strata(toy_reads, size = 10,
#'                                   by = c("Q1", "more"))
#'
#' Example 3: Stratified Sampling with Unequal Sample Sizes per Stratum
#' stratified_df3 <- textsamp_strata(toy_reads, size = 10,
#'                                   by = "more", equal_size_samples = FALSE)
#'
#' @export

textsamp_strata <- function(x, size, by = NULL, equal_size_samples = FALSE, ...) {

  # Input validation
  if ( missing( x ) )
    stop("Argument 'x' (dataframe or corpus) is missing.")
  if ( missing( size ) )
    stop("Argument 'size' (number of documents to sample) is missing.")
  if ( !is.null( by ) && !inherits( by, c( "data.frame", "character" ) ) ) {
    stop( "'by' must be a data.frame or a character vector of docvar names." )
  }

  # Handle Stratification Variables
  if ( is.character( by ) ) {
    stratum_vars <- by
  } else {
    stopifnot( all( by %in% colnames( x ) ) ) # Ensure 'by' variables are in 'x'
  }

  # Count unique combinations of stratification variables
  k <- x %>% distinct( across( all_of( stratum_vars ) ) ) %>% nrow()

  # Prepare data for sampling within strata
  x_grouped <- x %>% group_by( across( all_of( stratum_vars ) ) )

  # Choose between equal or proportional sampling
  if ( equal_size_samples ) {
    # Equal-size sampling
    n_k <- round( size / k ) # Sample size per stratum for equal sampling
    sampled_data <- x_grouped %>% slice_sample( n = n_k ) # Equal sampling
  } else {

    # Proportional sampling
    sampled_data <- x_grouped %>%
      sample_n(size = round((n() / nrow(x)) * size)) %>%
      ungroup()

    # props <- x_grouped %>% summarize(prop = n() / nrow(x))  # Calculate proportions
    # sampled_data <- x_grouped %>%
    #   sample_n(size = round(props$prop[cur_group_id()] * size)) %>%
    #   ungroup()
  }
  return( sampled_data )
}



#' @rdname textsamp
#'
#'   This method will attempt to sample an equal number of documents
#'   from each cluster defined by the unique combination of the passed
#'   clustering variables.
#'
#'   In particular, this method will sample $round( size / K )$
#'   documents from each cluster, where K is the total number of
#'   clusters.
#'
#'   If multiple clustering variables are passed, this will make
#'   clusters as all the _unique_ combinations of these variables.
#'   E.g., if A has values of 1, 2, 3 and B has values of 1, 2, then
#'   there could be up to six clusters.
#'
#' @param by a \code{data.frame} with document-level grouping
#'   variable(s) or character vector with names of variables in
#'   `docvars(x)`
#' @param ... additional arguments passed on to `textsamp`. Cannot
#'   include `scheme`.
#' @export
textsamp_cluster <- function(x, size, by = NULL, ...) {
  # Input validation (same as in textsamp_strata)
  if (missing(x)) stop("Argument 'x' (dataframe or corpus) is missing.")
  if (missing(size)) stop("Argument 'size' (number of documents to sample) is missing.")
  if (!is.null(by) && !inherits(by, c("data.frame", "character"))) {
    stop("'by' must be a data.frame or a character vector of docvar names.")
  }




  if (is.null(by)) {
    # If no clustering variables are provided, treat all documents as one cluster
    return(textsample(x, size = size, ...))
  }

  if (is.data.frame(by)) {
    x <- cbind(x, by)  # Combine data and clustering variables if 'by' is a dataframe
    cluster_vars <- colnames(by)
  } else {
    if (!all(by %in% colnames(x))) {
      stop("Cluster variables not found in the data.")
    }
    cluster_vars <- by
  }

  # Count unique clusters
  num_clusters <- x %>%
    select(all_of(cluster_vars)) %>%
    distinct() %>%
    nrow()

  # Calculate sample size per cluster
  sample_per_cluster <- round(size / num_clusters)

  # Sample within each cluster
  sampled_data <- x %>%
    group_by(across(all_of(cluster_vars))) %>%
    slice_sample(n = sample_per_cluster) %>%
    ungroup()

  # If x is a corpus, convert sampled_data back to corpus
  if (quanteda::is.corpus(x)) {
    sampled_data <- quanteda::corpus(sampled_data)
  }

  return(sampled_data)
}



