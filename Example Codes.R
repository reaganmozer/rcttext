library(rcttext)
### Remember

# generate_features is dependent on clean_features.
# apply_hunsepp is dependent on repair_spelling.
# extract_liwc is dependent on the LIWC program.
# extract_tacco is dependent on the tacco program.
# apply_hunsepll's to_lower parameter? similar with clean_text 

### apply_hunspell example ----

## Example 1

# Text to be checked for spelling errors
txt = "This function seplaces the textt withh the suggestion"

# Check for spelling errors and apply replacements 
txt_rep = apply_hunspell( txt, verbose=TRUE, threshold = 0 )
txt_rep

## Example 2

# Load example dataframe with multiple texts
data("toy_reads")

# Extract texts from the dataframe
txts = toy_reads$text
length(txts)

# Check for spelling errors and apply replacements 
txts_rep = apply_hunspell( txts, verbose=TRUE, threshold = 0 )
which( txts_rep != txts )

# Display original and corrected texts
txts[7]
txts_rep[7]

txts[20]
txts_rep[20]

# Check for spelling errors and apply replacements with additional accepted words
txts_rep2 = apply_hunspell( txts,
                            verbose=TRUE,
                            additional_words = c( "dont", "rainforest" ),
                            threshold = 0 )
which( txts_rep2 != txts )

# Display original and corrected texts with additional accepted words
txts[7]
txts_rep2[7]

txts[20]
txts_rep2[20]

### genearate_features example ----

# This function does not work with the dataframe with 1 object. 
# 'meta' should be a dataframe. 

## Example 1

# Create a dataframe with 2 texts (objects). 
df = data.frame(
  text = c( "This function generates an array of text features", 
            "This function generates a rich feature representation as a character 
             vector or quanteda::corpus() object by applying an array of linguistic 
             and syntactic indices" ))

# Generated text features without simplfying the set of features 
feats1 = generate_features( df$text, 
                            meta = df,  
                            clean_features = FALSE )


## Example 2

# Load example dataframe with multiple texts
data( "toy_reads" )

# Generate text features without simplifying the set of features
feats2 = generate_features( toy_reads$text, 
                            meta=toy_reads, 
                            clean_features = FALSE, 
                            ignore = "ID" )

# Generate preliminary text features, 
# simplifying the set of features and specifying sent, read, ld
feats3 = generate_features( toy_reads$text, meta=toy_reads, 
                            sent = TRUE,
                            clean_features = TRUE,
                            read = c("Flesch","Flesch.Kincaid", "ARI"),
                            ld=c("TTR","R","K"),
                            ignore=c("ID"),
                            verbose = TRUE )

### clean_text example ----

## Typo correction: given copus x -> given corpus

# Texts to be cleaned
txts = c( "THIS FUNCTION CONVERTS EVERYTHING TO LOWERCASE.",
          "This function removes punctuation........", 
          "This function removes     whitespace.",
          "This-function-splits-hyphens",
          "The main use for clean_text is to check which elements of 
           your text are converted to empty strings.",
          " " )

## Example 1 (convert to lowercase)
txts[1]
clean_text( txts[1] )

## Example 2 (remove punctuation)
txts[2]
clean_text( txts[2] )

## Example 3 (remove whitespace)
txts[3]
clean_text( txts[3] )

## Example 4 (split hyphens)
txts[4]
clean_text( txts[4] )
clean_text( txts[4], split_hyphens = FALSE )

## Example 5 (clean entire vector of texts)
clean_text( txts )

### repair_spelling example ----

## Example 1

# Texts to be repaired
txt = "This function replaces alll wordss in dictionary with alternates"

# Repair spelling errors by replacing specified words with their correct forms
txt_rep = repair_spelling( txt,
                           c( "alll", "wordss" ),
                           c( "all", "word" ))
txt_rep

## Example 2
data( "toy_reads" )

# Repair spelling errors in the text column of the dataframe

toy_reads$text_rep = repair_spelling( toy_reads$text,
                                      c( "No", "the", "what","My" ),
                                      c( "NO.", "THE", "WHAT", "MY" ) )

# View the original and repaired texts
view(data.frame(toy_reads$text, toy_reads$text_rep))

#  No did not change due to the defined function. (No starts in the beginning or No, No!)
#  dictionary = paste0( " ", dictionary, " " )
#  to_words = paste0( " ", to_words, " " )

### extract_liwc example ----

# This function extracts features from a LIWC-generated CSV file.
# The txt_data should be a dataframe and have the same column name(s) as the LIWC file.
all.feats = extract_liwc( file = "liwc.csv",
                          meta = txt_data,
                          ID.liwc = c( "ID" ),
                          ID.meta = c( "ID" ),
                          clean = FALSE )

### extract_tacco example ----

# This function extracts features from a TAACO-generated CSV file.
# The txt_data should be a dataframe and have the same column name(s) as the TAACO file.
all.feats = extract_taaco( "taaco.csv",
                            meta = txt_data,
                            ID.meta = "ID" )

### extract_w2v example ----

# This function compute document-level features using a word2vec model.
# The txt_data should be a dataframe
all.feats = extract_w2v( clean_text( txt_data$text ),
                         meta = txt_data,
                         model = glove.50d )


