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
#' @param sampling the type of sampling to perform. Options include 'strata' and 'cluster'.
#' @param sampling_control A list containing control parameters
#'   for the specified sampling method (see details)
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
#' @details
#' For stratified sampling (`sampling = "strata"`), `sampling_control` should be a list with:
#'   * `by`: A character vector of column names in `x` to define the strata.
#'   * `equal_size_samples`: Logical. If TRUE, sample equal numbers from each stratum.
#'
#' For cluster sampling (`sampling = "cluster"`), `sampling_control` should be a list with:
#'   * `by`: A character vector of column names in `x` to define the clusters.
#'   * `n_clusters`: The number of clusters to sample.
#'   * `cluster_size`: (Optional) The number of documents to sample from each cluster.
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

data(swissmunicipalities)
x = swissmunicipalities
size = 30
prob = NULL
wt.fn = NULL
scheme = NULL
sampling = "strata"
sampling_control = list(by = 'REG', equal_size_samples = TRUE)
method = "srswor"


textsamp <- function(x,
                     size = NULL, prob = NULL, wt.fn = NULL, scheme = NULL,
                     sampling = NULL,
                     sampling_control = NULL,
                     method = c( "srswor", "srswr", "systematic", "poisson" ),
                     return.data = TRUE, ... ) {
  # Match the specified method with available options
  method <- match.arg( method,
                       choices = c( "srswor", "srswr", "systematic", "poisson" ))

  # Check if the specified sample size is valid
  if (is.null(size)) stop("Argument 'size' (number of documents to sample) is missing.")
  if( size < 0 || size > nrow(x) ){
    stop("Invalid sample size. Size must be non-negative and less than the number of documents.")
  }

  # Generate probability weights using the provided function if prob is NULL
  if ( !is.null( wt.fn ) && is.null( prob ) ) {
    prob <- wt.fn(x)
  }

  # Sampling Logic
  if (is.null(sampling)) {
    # No stratification or clustering
    x <- x

  } else {
    # Stratified or clustered sampling
    if (sampling == "strata") {
      if (is.null(sampling_control)) stop("`sampling_control` must be provided for stratified sampling.")
      x <- textsamp_strata(x, size, by = sampling_control$by,
                           equal_size_samples = sampling_control$equal_size_samples)

    } else if (sampling == "cluster") {
      if (is.null(sampling_control)) stop("`sampling_control` must be provided for cluster sampling.")
      if (is.null(sampling_control$n_clusters)) {
        stop("Argument 'n_clusters' (number of clusters to sample) is required for clustered sampling.")
      }
      x <- textsamp_cluster(x, by = sampling_control$by,
                            n_clusters = sampling_control$n_clusters,
                            cluster_size = sampling_control$cluster_size)
    } else {
      stop("Invalid sampling type. Choose 'strata' or 'cluster'.")
    }
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
#'   documents in stratum $k$. If these numbers do not divide, this
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
  if ( !is.null( by ) && !inherits( by, c( "variable", "character" ) ) ) {
    stop( "'by' must be variable(s) of a data.frame or a character vector of docvar names." )
  }

  # Handle stratification variables
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
#'   This method will attempt to sample certain number of clusters of documents
#'   from data (simple random clustering) at first
#'   and then sampling an equal number of documents from each selected cluster.
#'   defined by the unique combination of the passed clustering variables.
#'
#'   (In particular, this method will sample $round( size / K )$
#'   documents from each cluster, where K is the total number of
#'   clusters.) - sounds like equal_size_samples within stratified sampling - n
#'
#'   If multiple clustering variables are passed, this will make
#'   clusters as all the _unique_ combinations of these variables.
#'   E.g., if A has values of 1, 2, 3 and B has values of 1, 2, then
#'   there could be up to six clusters.
#'
#' @param by a \code{data.frame} with document-level grouping
#'   variable(s) or character vector with names of variables in
#'   `docvars(x)`
#' @param n_clusters The number of clusters to sample.
#'   If not provided, an error will be thrown.
#' @param size The number of documents to sample across all clusters.
#'   If NULL, all documents will be sampled from each selected cluster.
#' @param n_cluster the number of clusters to sample from data
#' @param cluster_size The number of documents to sample from each selected cluster.
#' If NULL, all documents within each cluster will be sampled.
#' @param ... additional arguments passed on to `textsamp`. Cannot
#'   include `scheme`.
#' @export

textsamp_cluster <- function(x, by = NULL, n_clusters, cluster_size = NULL, ...) {

  # Input validation
  if (missing(x)) {
    stop("Argument 'x' (corpus object or dataframe) is missing.")
  }
  if (missing(n_clusters)) {
    stop("Argument 'n_clusters' (number of clusters to sample) is missing.")
  }
  if ( !is.null( by ) && !inherits( by, c( "variable", "character" ) ) ) {
    stop( "'by' must be a variable(s) of a data.frame or a character vector of docvar names." )
  }

  # Handle clustering variables
  if (is.character(by)) {
    cluster_vars <- by
  } else {
    stopifnot(all(by %in% colnames(x)))
  }

  # Get unique cluster IDs
  clusters <- x %>% distinct(across(all_of(cluster_vars)))

  # Check if enough clusters exist for sampling
  if(n_clusters > nrow(clusters)) {
    stop("There are not enough unique clusters in the data to sample the specified number.")
  }

  # Sample the specified number of clusters
  sampled_clusters <- clusters %>% slice_sample(n = n_clusters)

  # Sample documents (Within or Across Clusters)
  if (is.null(cluster_size)) {
    # Sample all documents from the selected clusters
    sampled_data <- x %>%
      right_join(sampled_clusters, by = cluster_vars) %>%
      group_by(across(all_of(cluster_vars))) %>%
      ungroup()

   } else {
     # Sample a fixed number of documents within each selected cluster
      sampled_data <- x %>%
        right_join(sampled_clusters, by = cluster_vars) %>%
        group_by(across(all_of(cluster_vars))) %>%
        slice_sample(n = cluster_size) %>%
        ungroup()
    }

  return(sampled_data)
}
