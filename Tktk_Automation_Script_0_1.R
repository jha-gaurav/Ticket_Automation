#This R-script is for ticket automation and consists of teh following parts.
#1. Importing packages and setting global variables
#2. Getting the Data
#3. Cleaning the data - Create Corpus
#4. Create a DFM from the cleaned corpus
#5. Convert the DFN to a tf-idf matrix


##########################################################################################
### SCRIPT START #########################################################################
##########################################################################################

###################################################
#1. Importing packages and setting global variables
###################################################

set.seed(1000)
checkPackage <- function(pkgName){
  
  if (!pkgName %in% installed.packages()) {
    install.packages(pkgName)
  }
  library(pkgName, character.only = TRUE)
}

#Add any packages into the vector packages and rerun the nullist <- lapply(packages, checkPackage) again
packages <- c("ggplot2", "caret", "e1071", "randomForest", "quanteda", "irlba", "xlsx")
nullist <- lapply(packages, checkPackage)



###################################################
#2. Getting the Data
###################################################

#Currently this section is not executd as there is no data.

# The data output of this step will be a data.frame with the following fields
#Tkt_Num, i.e. ticket number alotted by the system
#User, i.e. Name of the user who reported the incident
#Date_Raised, i.e. Date the Issue was reported
#Issue_Summ, i.e. Brief description of the problem
#Comments, i.e. Further commentry on the resolution provided

#The output dataframe will be init.df

init.df <- data.frame(1, "User_1", "12/01/2018", "Issue Summ 1 this is the problem faced", "Comments 1 all comms", stringsAsFactors = FALSE)
names(init.df) <- c("Tkt_Num", "User", "Date_Raised", "Issue_Summ", "Comments")

###################################################
#3. Cleaning the data - Create Corpus
###################################################

#Cleaning the data in init.df

#Create a corpus by concatenating the Issue_Summ and Comments column

#Create a new data frame using only the Tkt_Num

temp_df <- subset(init.df, select = "Tkt_Num")
temp_df$issue_txt <- paste(init.df$Issue_Summ, init.df$Comments, sep = " ")

#Now a data frame is created which has only the ticket number and issue details.
#We can start with the text cleaning and tokenizing

init.tokens <- tokens(temp_df$issue_txt, what = "word", remove_numbers = TRUE,
                      remove_punct = TRUE, remove_symbols = TRUE, remove_hyphens = TRUE)
init.tokens <- tokens_ngrams(init.tokens, n = 1:3)

#Convert to lower case
init.tokens <- tokens_tolower(init.tokens)

#Remove Stopwords
init.tokens <- tokens_select(init.tokens, stopwords(), selection = "remove")

#Perform token stemming
init.tokens <- tokens_wordstem(init.tokens, language = "english")

#The tokens are now created. We can move ahead to creating DFM

###################################################
#4. Create a DFM from the cleaned corpus
###################################################

init.tokens.dfm <- dfm(init.tokens, tolower = FALSE)
init.tokens.matrix <- as.matrix(init.tokens.dfm)


###################################################
#5. Convert the DFN to a tf-idf matrix
###################################################

term.frequency <- function(row) {
  row / sum(row)
}

# Our function for calculating inverse document frequency (IDF)
inverse.doc.freq <- function(col) {
  corpus.size <- length(col)
  doc.count <- length(which(col > 0))
  
  log10(corpus.size / doc.count)
}

# Our function for calculating TF-IDF.
tf.idf <- function(x, idf) {
  x * idf
}

#Calculate the term-frequency for the matrix
init.tokens.tf <- apply(init.tokens.matrix, 1, term.frequency)

#Calculate the inverse-document-frequency for the matrix
init.tokens.idf <- apply(init.tokens.matrix, 2, inverse.doc.freq)

#Calculate the TF-IDF for the matrix
init.tokens.tfidf <- apply(init.tokens.tf, 2, tf.idf, idf = init.tokens.idf)


#Transpose the matrix
init.tokens.tfidf <- t(init.tokens.tfidf)

#Check an d fix for incomplete cases
incomplete.cases <- which(!complete.cases(init.tokens.tfidf))
init.tokens.tfidf[incomplete.cases, ] <- rep(0.0, ncol(init.tokens.tfidf))
