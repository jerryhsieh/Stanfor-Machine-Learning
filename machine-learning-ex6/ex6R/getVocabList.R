getVocabList <- function () {
#GETVOCABLIST reads the fixed vocabulary list in vocab.txt and returns a
#cell array of the words
#   vocabList = GETVOCABLIST() reads the fixed vocabulary list in vocab.txt 
#   and returns a cell array of the words in vocabList.


# Read the fixed vocabulary list
#fid = fopen('vocab.txt')
f <- read.table("../ex6/vocab.txt", sep="\t", col.names= c("id", "txt"), stringsAsFactors=FALSE)
    
# Store all dictionary words in cell array vocab{}
#n = 1899  # Total number of words in the dictionary
n <- length(f)

# For ease of implementation, we use a struct to map the strings => integers
# In practice, you'll want to use some form of hashmap
#vocabList = cell(n, 1);
#for (i in 1:n) {
    # Word Index (can ignore since it will be = i)
    #scanf(f, '%d', 1)
    # Actual Word
    #vocabList[i] = f$txt[i]
#}

#fclose(fid);

#simply return f$txt

    return(f$txt)
}
