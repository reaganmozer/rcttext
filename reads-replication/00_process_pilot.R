## Process pilot data (a subset of G1 science essays from a different
## evaluation) that we will use to train the ML model

source( here::here( "reads-replication/setup.R" ) )

pilot = read.csv( reads_file_path("data/write_machinelearning_pilot_replication.csv") )

if ( FALSE ) {
  # For testing
  # Cut down for illustration purposes (make everything faster)
  set.seed(103403)
  pilot = slice_sample( pilot, n=20 )
}

pilot <- pilot %>% mutate(ID=row_number())

# Grab key columns of metadata
all.feats = select(pilot, ID, writing_quality_score_1,
                   writing_quality_score_2, more)

# Get text (and repair one piece of spelling)
essay.text = pilot$response %>%
  repair_spelling( "shoud", "should" )

# Generate set of general features
all.feats = generate_features(essay.text,
                              meta = all.feats,
                               sent = TRUE,
                              clean_features = FALSE,
                              terms = "xxx",
                              read = c("Flesch","Flesch.Kincaid", "ARI", "ELF",
                                       "meanWordSyllables"),
                              verbose = TRUE )

# Note: term of 'xxx' are illegible words/phrases
table( all.feats$xxx )

# Add Word2Vec projections for each essay on 50 dimensions
load( "data-raw/glove.50d.RData" )
all.feats = extract_w2v( clean_text(essay.text),
                         meta = all.feats,
                         model = glove.50d )



# Add externally computed LIWC-generated features
all.feats <- extract_liwc( reads_file_path("Generated Data/pilot/pilot_LIWC.csv" ),
              meta = all.feats, ID.liwc = 1, ID.meta = "ID",
              clean = FALSE )


# And externally computed TAACO features
all.feats <- extract_taaco( reads_file_path("Generated Data/pilot/pilot_taaco_results.csv"),
                            meta = all.feats,
                            ID.meta = "ID" )


# Drop features we don't need/that are redundant
dim( all.feats )
all.feats = clean_features( all.feats,
                            ignore = c( "ID", "Q1", "Q2", "more" ) )

dim( all.feats )

save( all.feats,
      file=reads_file_path("Generated Data/all.pilot.RData") )
