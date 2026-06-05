## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(tidyverse)
knitr::opts_chunk$set(echo = TRUE,
                      fig.width = 5,
                      fig.height = 3,
                      out.width = "75%",
                      message = FALSE,
                      fig.align = "center")
options(list(dplyr.summarise.inform = FALSE))
theme_set( theme_classic() )


## ----install, eval=FALSE------------------------------------------------------
# devtools::install_github("https://github.com/reaganmozer/rcttext/")

## ----setup, message=FALSE-----------------------------------------------------
library(rcttext)

## -----------------------------------------------------------------------------
data( "toy_reads" )
names( toy_reads )
table( toy_reads$more )
table( toy_reads$Q1 )

## -----------------------------------------------------------------------------
features <- generate_features( toy_reads$text )
dim( features )

# You can tweak how it runs
features <- generate_features( toy_reads$text,
                               sent = TRUE,
                               clean_features = FALSE,
                               terms = c( "xxx", "monkeys" ),
                               read = c("Flesch","Flesch.Kincaid", "ARI",
                                        "ELF", "meanWordSyllables"),
                               ld=c("TTR","R","K"),
                               ignore="s_id",
                               verbose = TRUE )


dim( features )
head( features )

summary( features$monkeys )
class( features )

# And then drop colinear features, etc
features_clean = clean_features( features, ignore = "monkeys" )
class( features_clean )
dim( features )

## ----eval=T-------------------------------------------------------------------
# glove.50d = textdata::embedding_glove6b(dimensions = 50)

features = extract_w2v( clean_text(toy_reads$text),
                        meta = features,
                        model = mini_glove)
dim( features )

## -----------------------------------------------------------------------------
ref_docs = c( "Monkeys should live! Trees and birds are also important.  Trees should not be cut down as they are houses for animals",
              "Unrelated gibberish is not good" )
ref_docs = clean_text( ref_docs )

features = generate_distance_features( clean_text(toy_reads$text),
                                       features,
                                       ref_docs,
                                       method="cosine" )
tail( colnames( features ) )
summary( features$doc_1 )
summary( features$doc_2 )


## ----impact_estimation, warnings=FALSE----------------------------------------
features_clean = clean_features( features, ignore = c( "monkeys" ) )
dim( features )
dim( features_clean )

all <- impacts_on_features( features,
                            ignore = "s_id",
                            meta = toy_reads,
                            formula = ~ more,
                            mcp = "fdr" )

# one row per feature of analysis
head(all)

# All the p-values (unadjusted)
ggplot( all, aes( p.value ) ) +
  geom_dotplot( binwidth = 0.025 ) +
  expand_limits( x = 0 ) +
  theme_minimal()


# All the p-values (adjusted)
ggplot( all, aes( p.adj ) ) +
  geom_dotplot( binwidth = 0.025 ) +
  expand_limits( x = 0 ) +
  theme_minimal()

head( all )

all_sub = filter( all, p.value <= 0.20 )
plot_textfx( all_sub, main = "Impact Plot" )

## ----warnings=FALSE-----------------------------------------------------------

cwords_untaught <- c("potential", "unique", "camouflage", "diversity", "carnivore",
                     "hypothesis", "organism", "trait", "reptile")
cwords_taught <- c("survive", "species", "behavior", "advantage", "adaptation",
                   "habitat", "physical_feature", "extinct", "fossil", "brutal",
                   "evidence", "theory", "hunter", "paleontologist")
monkey <- c( "monkey", "monkeys" )

# This function returns a line of statistics for term frequencies
r1 <- textfx_terms( toy_reads$text, toy_reads$more, cwords_untaught )
r2 <- textfx_terms( toy_reads$text, toy_reads$more, cwords_taught )
r3 <- textfx_terms( toy_reads$text, toy_reads$more, monkey )
bind_rows( untaught = r1, taught = r2, monkey = r3, .id="group" ) %>%
  dplyr::select( -termfreq_1, -termfreq_0, -LL, -UL ) %>%
  knitr::kable( digits=2 )

monkey = str_detect( toy_reads$text, "monkey" )
table( has_monkey = monkey, toy_reads$more )


## -----------------------------------------------------------------------------
# We can fit a series of related CCS models and look for stability
# across models of different phrases.
toy_reads$clean_text = clean_text( toy_reads$text )
m1 = ccs_tuned_textreg( corpus = toy_reads$clean_text,
                        Z = toy_reads$more,
                        R = 20 )
m2 = ccs_tuned_textreg( corpus = toy_reads$clean_text,
                        Z = toy_reads$more,
                        R = 20,
                        Lq = 4 )
m3 = textreg::textreg( corpus = toy_reads$clean_text,
                       labeling = 2*toy_reads$more - 1,
                       verbosity = 0,
                       banned = c( "monkeys", "apes", "love"),
                       C = 4 )

mods = list( classic = m1, L4 = m2, curated = m3 )

# We have two different kinds of tables of results, that aggregate
# across models
ctbl <- ccs_list_table( mods )
ctbl

rtbl <- ccs_result_table( mods, toy_reads$clean_text,
                          toy_reads$more )
rtbl

# We also have a plot of differential use.
plot_ccs( rtbl )


## -----------------------------------------------------------------------------
# making some 'pilot' data
data( toy_reads )
tt = dplyr::bind_rows(toy_reads, toy_reads, toy_reads )
tt$text = paste( tt$text,
                 sample( c("dog", "cat", "pig", "cow", tt$text), nrow(tt), replace = TRUE ),
                 sample( c( "cow", "pig", "cat", "goat"), nrow(tt), replace = TRUE ), sep = " " )
tt$Q2 = tt$Q2 + rnorm( nrow(tt) )
pilot_feat = generate_features( tt$text, lex = TRUE, sent = FALSE )

mods <- train_models( pilot_feat, tt$Q2, methods = "small",
                      include_BART = FALSE, verbose = FALSE )
names(mods)

## -----------------------------------------------------------------------------
feats = generate_features( toy_reads$text, lex = TRUE, sent = FALSE, clean_features = FALSE ) %>%
  dplyr::select( all_of( colnames( pilot_feat ) ) )
preds = generate_predictions( mods, feats, toy_reads[ "more" ] )
head( preds )

## -----------------------------------------------------------------------------
estimatr::lm_robust( mod_stack ~ more, data = preds ) %>%
  broom::tidy() %>%
  dplyr::select( -outcome ) %>%
  knitr::kable(digits=2)

