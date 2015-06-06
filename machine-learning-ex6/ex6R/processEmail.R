processEmail <- function (email_contents) {
#PROCESSEMAIL preprocesses a the body of an email and
#returns a list of word_indices 
#   word_indices = PROCESSEMAIL(email_contents) preprocesses 
#   the body of an email and returns a list of indices of the 
#   words contained in the email. 
#

# Load Vocabulary
library(SnowballC)   # for Porter word stemmer    
source("getVocabList.R")
vocabList = getVocabList()

# Init return value
word_indices = integer()

# ========================== Preprocess Email ===========================
    
# Find the Headers ( \n\n and remove )
# Uncomment the following lines if you are working with raw emails with the
# full headers

# hdrstart = strfind(email_contents, ([char(10) char(10)]));
# email_contents = email_contents(hdrstart(1):end);

# Lower case
email_contents = tolower(email_contents)

#Strip all HTML
# Looks for any expression that starts with < and ends with > and replace
# and does not have any < or > in the tag it with a space
email_contents = gsub('<[^<>]+>', ' ', email_contents, perl=T)

# Handle Numbers
# Look for one or more characters between 0-9
email_contents = gsub('[0-9]+', 'number', email_contents, perl=T)

# Handle URLS
# Look for strings starting with http:// or https://
email_contents = gsub('(http|https)://[^\\s]*', 'httpaddr', email_contents, perl=T)

# Handle Email Addresses
# Look for strings with @ in the middle
email_contents = gsub('[^\\s]+@[^\\s]+', 'emailaddr', email_contents, perl=T)

# Handle $ sign
email_contents = gsub('[$]+', 'dollar', email_contents, perl=T)

# get rid of punctuation, leave only alphanumeric and whitespace
email_contents = gsub( "[^[:alnum:] ]", " ", email_contents )



# ========================== Tokenize Email ===========================
    
# Output the email to screen as well
message(sprintf('\n==== Processed Email ====\n\n'))

# Process file


str <- unlist(strsplit(email_contents, " "))

l <- 0
for (i in 1:length(str) ) {

# Tokenize and also get rid of any punctuation
#    [str, email_contents] = strtok(email_contents, [' @$/#.-:&*+=[]?!(){},''">_<;%' char(10) char(13)])

# Remove any non alphanumeric characters
#    str = regexprep(str, '[^a-zA-Z0-9]', '');
    
    
# Stem the word 
# (the porterStemmer sometimes has issues, so we use a try catch block)
    out <- tryCatch (
        { 
            trim <- function (x) gsub("^\\s+|\\s+$", "", x)
            #str = porterStemmer(trim(str[i]))
            tok = wordStem(trim(str[i]), language = "porter")
        }, 
        error = function(cond) {
            message("porterStemmer return error ")
            message(cond)
        },
        warning = function(cond) {
            message ("porterStemmer return warning ")
            message(cond)
        },
        finally = {
            # message("porterStemmer return sucessfully")            
        }
    )
    
    #message(sprintf(" now i = %d and str = %s", i, tok))
# Skip the word if it is too short
    if (length(tok) < 1 || is.na(tok)) {
        message(sprintf(" get strange tok %s", tok))
        next
    }

# Look up the word in the dictionary and add to word_indices if
# found
# ====================== YOUR CODE HERE ======================
# Instructions: Fill in this function to add the index of str to
#               word_indices if it is in the vocabulary. At this point
#               of the code, you have a stemmed word from the email in
#               the variable str. You should look up str in the
#               vocabulary list (vocabList). If a match exists, you
#               should add the index of the word to the word_indices
#               vector. Concretely, if str = 'action', then you should
#               look up the vocabulary list to find where in vocabList
#               'action' appears. For example, if vocabList{18} =
#               'action', then, you should add 18 to the word_indices 
#               vector (e.g., word_indices = [word_indices ; 18]; ).
# 
# Note: vocabList{idx} returns a the word with index idx in the
#       vocabulary list.
# 
# Note: You can use strcmp(str1, str2) to compare two strings (str1 and
#       str2). It will return 1 only if the two strings are equivalent.
#

#    for (j in 1:length(vocabList)) {
#        if (strcmp(str, vocabList[j])) {
#            word_indices = cbind(word_indices, j)
#            break
#        endif
#    }

    if (j <- match(tok, vocabList, nomatch = 0))  {
        word_indices = c(word_indices, j)
    }

# =============================================================
    
    
# Print to screen, ensuring that the output lines are not too long
    if ((l + length(tok) + 1) > 78) {
        message(sprintf('\n'))
        l = 0
    }
    cat(sprintf('%s ', tok))
    l = l + length(tok) + 1

}

# Print footer
message(sprintf('\n\n=========================\n'))

return(word_indices)


} 