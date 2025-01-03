% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/textsamp.R
\name{textsamp}
\alias{textsamp}
\alias{textsamp_strata}
\alias{textsamp_cluster}
\title{Select a random sample of documents}
\usage{
textsamp(
  x,
  size = NULL,
  prob = NULL,
  wt.fn = NULL,
  scheme = NULL,
  sampling = NULL,
  sampling_control = NULL,
  method = c("srswor", "srswr", "systematic", "poisson"),
  return.data = TRUE,
  ...
)

textsamp_strata(x, size, by = NULL, equal_size_samples = FALSE, ...)

textsamp_cluster(x, by = NULL, n_clusters, cluster_size = NULL, ...)
}
\arguments{
\item{x}{A \link{corpus} object or character vector of text
documents or dataframe.}

\item{size}{The number of documents to sample across all clusters.
If NULL, all documents will be sampled from each selected cluster.}

\item{prob}{a vector of probability weights for each document.}

\item{wt.fn}{a function for generating probability weights; ignored
when \code{prob} is used. See Details.}

\item{scheme}{optional sampling scheme to implement.  NOT YET
IMPLEMENTED.}

\item{sampling}{the type of sampling to perform. Options include 'strata' and 'cluster'.}

\item{sampling_control}{A list containing control parameters
for the specified sampling method (see details)}

\item{method}{the following methods are implemented: simple random
sampling without replacement (`srswor`), simple random sampling
with replacement (`srswr`), Poisson sampling (`poisson`),
systematic sampling (`systematic`); if \code{method} is missing,
the default method is \code{srswor}.}

\item{return.data}{logical; if \code{TRUE}, the function returns
the subset of x that are sampled. FALSE returns a vector of row
numbers corresponding to the sampled documents.}

\item{...}{additional arguments passed on to `textsamp`. Cannot
include `scheme`.}

\item{by}{a \code{data.frame} with document-level grouping
variable(s) or character vector with names of variables in
`docvars(x)`}

\item{equal_size_samples}{TRUE means sample same number of
documents from each strata.  FALSE means sample proportional to
strata size, as described above.}

\item{n_clusters}{The number of clusters to sample.
If not provided, an error will be thrown.}

\item{cluster_size}{The number of documents to sample from each selected cluster.
If NULL, all documents within each cluster will be sampled.}

\item{n_cluster}{the number of clusters to sample from data}
}
\value{
Returns either the sampled data or a vector of rownumbers
  of sampled documents.
}
\description{
Functions to select random samples of documents using different sampling schemes and/or
along different design criteria.

Proportions will be calculated as round( size * n_k / n ), where
  n is the number of documents total.

(In particular, this method will sample $round( size / K )$
  documents from each cluster, where K is the total number of
  clusters.) - sounds like equal_size_samples within stratified sampling - n
}
\details{
Select a random sample of documents

Functions to select random samples of documents using different
sampling schemes and/or along different design criteria.


For stratified sampling (`sampling = "strata"`), `sampling_control` should be a list with:
  * `by`: A character vector of column names in `x` to define the strata.
  * `equal_size_samples`: Logical. If TRUE, sample equal numbers from each stratum.

For cluster sampling (`sampling = "cluster"`), `sampling_control` should be a list with:
  * `by`: A character vector of column names in `x` to define the clusters.
  * `n_clusters`: The number of clusters to sample.
  * `cluster_size`: (Optional) The number of documents to sample from each cluster.

If multiple clustering variables are passed, this will make
  clusters as all the _unique_ combinations of these variables.
  E.g., if A has values of 1, 2, 3 and B has values of 1, 2, then
  there could be up to six clusters.
}
\examples{

# Load example dataframe
data("toy_reads")

## Example 1:Sample 4 documents using the default method
##           (simple random sampling without replacement)
textamp_df = textsamp(toy_reads, size = 4)

## Example 2: Sample 8 documents using Poisson sampling
textamp_df2 = textsamp(toy_reads, size = 8, method = "poisson")

## Example 3: Sample 8 documents using systematic sampling,
##            but only return the row numbers
textamp_df3 = textsamp(toy_reads, size = 8,
                       method = "systematic", return.data = FALSE)

# Load example dataframe
data("toy_reads")

## Example 1: Stratified Sampling by a Single Variable
stratified_df1 <- textsamp_strata(toy_reads, size = 6, by = "more")

Example 2: Stratified Sampling by Multiple Variables
stratified_df2 <- textsamp_strata(toy_reads, size = 10,
                                  by = c("Q1", "more"))

Example 3: Stratified Sampling with Unequal Sample Sizes per Stratum
stratified_df3 <- textsamp_strata(toy_reads, size = 10,
                                  by = "more", equal_size_samples = FALSE)

}
