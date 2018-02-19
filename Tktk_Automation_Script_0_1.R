#This R-script is for ticket automation and consists of the following parts.
#Phase-0: Setup
#1. Importing packages and setting global variables

#Phase-1: Getting the training data
#2. Getting the Data

#Phase-2: Clean the data and create dfm
#3. Cleaning the data - Create Corpus
#4. Create a DFM from the cleaned corpus
#5. Convert the DFN to a tf-idf matrix

#Phase-3: Get the test data and convert it into the usable format


#Phase-4: Use the cleaned test data to find the matches from training data




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

#Add any packages into the vector packages
#Rerun the nullist <- lapply(packages, checkPackage) again

packages <- c("ggplot2", "caret", "e1071", "randomForest", "quanteda", 
              "irlba", "xlsx", "lsa")
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

#The output dataframe will be train.df

train.df <- data.frame(1, "User_1", "12/01/2018", "Issue123 Summ this is the 
                      problem faced", "Issue123 Comments all comms", 
                      stringsAsFactors = FALSE)
names(train.df) <- c("Tkt_Num", "User", "Date_Raised", "Issue_Summ", "Comments")

###################################################
#3. Cleaning the data - Create Corpus
###################################################

#Cleaning the data in train.df

#Create a corpus by concatenating the Issue_Summ and Comments column

#Create a new data frame using only the Tkt_Num

temp_df <- subset(train.df, select = "Tkt_Num")
temp_df$issue_txt <- paste(train.df$Issue_Summ, train.df$Comments, sep = " ")

#Now a data frame is created which has only the ticket number and issue details.
#We can start with the text cleaning and tokenizing

train.tokens <- tokens(temp_df$issue_txt, what = "word", 
#                      remove_numbers = TRUE,
                      remove_punct = TRUE, 
                      remove_symbols = TRUE, 
                      remove_hyphens = TRUE)

#Create 3-gram tokens
train.tokens <- tokens_ngrams(train.tokens, n = 1:3)

#Convert to lower case
train.tokens <- tokens_tolower(train.tokens)

#Remove Stopwords
train.tokens <- tokens_select(train.tokens, stopwords(), selection = "remove")

#Perform token stemming
train.tokens <- tokens_wordstem(train.tokens, language = "english")

#The tokens are now created. We can move ahead to creating DFM

###################################################
#4. Create a DFM from the cleaned corpus
###################################################

train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)
train.tokens.matrix <- as.matrix(train.tokens.dfm)


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
train.tokens.tf <- apply(train.tokens.matrix, 1, term.frequency)

#Calculate the inverse-document-frequency for the matrix
train.tokens.idf <- apply(train.tokens.matrix, 2, inverse.doc.freq)

#Calculate the TF-IDF for the matrix
train.tokens.tfidf <- apply(train.tokens.tf, 2, tf.idf, idf = train.tokens.idf)


#Transpose the matrix
train.tokens.tfidf <- t(train.tokens.tfidf)

#Check and fix for incomplete cases
incomplete.cases <- which(!complete.cases(train.tokens.tfidf))
train.tokens.tfidf[incomplete.cases, ] <- rep(0.0, ncol(train.tokens.tfidf))


#Phase-3: Get the test data and convert it into the usable format

#1. The data will be read from a source and a dataframe will be created with
#the following fields
#Tkt_Num, i.e. ticket number alotted by the system
#User, i.e. Name of the user who reported the incident
#Date_Raised, i.e. Date the Issue was reported
#Issue_Summ, i.e. Brief description of the problem

#There will be no Comments field becayuse the test data will be a new ticket.

#The single (or multiple) documents in the test dataset will be in test.df

test.df <- data.frame(1, "User_1", "12/01/2018", "Issue123 Summ this is the 
                      problem faced", stringsAsFactors = FALSE)
names(test.df) <- c("Tkt_Num", "User", "Date_Raised", "Issue_Summ")

#Currently there is no data



#Phase-4: Use the cleaned test data to find the matches from training data

#Now that we have the test data in test.df, we need to process it in
#exactly the same manner as the training data to create similar features.

#Extract the Tkt_Num and Issue_Summ fields only from the dataframe.

temp_test_df <- subset(test.df, select = c("Tkt_Num", "Issue_Summ"))


#Do exactly same processing as done in training data to create tokens

test.tokens <- tokens(temp_test_df$Issue_Summ, what = "word", 
                       #                      remove_numbers = TRUE,
                       remove_punct = TRUE, 
                       remove_symbols = TRUE, 
                       remove_hyphens = TRUE)

#Create 3-gram tokens
test.tokens <- tokens_ngrams(test.tokens, n = 1:3)

#Convert to lower case
test.tokens <- tokens_tolower(test.tokens)

#Remove Stopwords
test.tokens <- tokens_select(test.tokens, stopwords(), selection = "remove")

#Perform token stemming
test.tokens <- tokens_wordstem(test.tokens, language = "english")

#Use dfm_select() function to create same features in the test dataset as in
#training dataset

test.tokens.dfm <- dfm(test.tokens, tolower = FALSE)

test.tokens.dfm <- dfm_select(test.tokens.dfm, pattern = train.tokens.dfm,
                              selection = "keep")
test.tokens.matrix <- as.matrix(test.tokens.dfm)

#Calculate the tf-idf for the test document

test.tokens.tf <- apply(test.tokens.matrix, 1, term.frequency)

test.tokens.tfidf <- apply(test.tokens.tf, 2, tf.idf, idf = train.tokens.idf)

#Transpose the matrix

test.tokens.tfidf <- t(test.tokens.tfidf)

#Fix Incomplete cases
test.tokens.tfidf[is.na(test.tokens.tfidf)] <- 0.0

#Use cosine() function to calculate the cosine similarity between the test data
#and the training matrix.


#Merge the Tkt_Num from training data and cosine similarity and sort on cosine
#values in descending order


#Return the Tkt_Num of top 5 in the sorted vector