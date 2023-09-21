#' Given text from a randomized trial with a binary treatment, this function computes
#' estimates for the average treatment effect with respect to an array of text-based outcomes
#'
#'
#' @param x A character vector of text documents or a feature matrix
#' @param Z Indicator for treatment assignment.
#' @param adj (optional) character vector or named list of variables in the data matrix to
#'   adjust for when estimating treatment impacts.
#' @param data A \code{data.frame} of subject-level identifiers, demographic
#'   variables, group membership, and/or other pre-treatment covariates.
#' @param design For multi-site and cluster randomized experiments, a named list of vectors containing site IDs and/or cluster IDs.
#' @param wts Sampling weights for documents.  Assumed uniform if null.
#' @param mcp character string specifying the correction method to be applied to adjust for multiple comparisons. Defaults to no adjustments. See \link{p.adjust} for available adjustment methods.
#' @return A model object for estimating treatment impact across an array of features.

#' @export

textfx <- function(x, Z,adj=NULL,data,
                   wts = NULL,
                   design=list(siteID=NULL, clusterID=NULL)){



}


#' Estimates the average treatment effect on the frequency and prevalence of a list of specified terms and phrases
#'
#'
#' @param x A character vector of text documents or a feature matrix
#' @param Z Indicator for treatment assignment.
#' @param terms Terms and phrases to evaluate
#' @param ... optional parameters passed to \link{quanteda::tokens()}
#' @return A vector showing the frequency and prevalence of the specified terms within each treatment group and results of the hypothesis test comparing prevalence across groups.

#' @export

textfx_terms = function(x, Z, terms, ...){

    # Determine maximum number of ngrams to search based on the input terms
    ngrams.terms = stringi::stri_count_words(terms)
    max.ngrams = 1:max(ngrams.terms)

    # Convert ngrams to quanteda structure with underscores
    terms = gsub(" ", "_", terms, fixed=T)

    dfm = quanteda::dfm(quanteda::tokens_ngrams(quanteda::tokens(x,...),n=max.ngrams))
    dfm.terms = as.matrix(quanteda::dfm_match(dfm, terms))

    out = data.frame(Z=Z, dfm.terms, tot=rowSums(dfm.terms))
    out = out %>% group_by(Z) %>% summarise(n=n(), termfreq=sum(tot), docfreq=sum(tot>0), prop.docs=docfreq/n) %>% arrange(desc(Z))

    # Hypothesis test for difference in proportions between groups
    test = prop.test(x=out$docfreq, n=out$n)


    out1 = out %>% pivot_wider(names_from=Z, values_from=!Z) %>%
      mutate(diff = test$estimate[1]-test$estimate[2],
             p.value = test$p.value,
             LL = test$conf.int[1],
             UL = test$conf.int[2])

    return(out1)

}


