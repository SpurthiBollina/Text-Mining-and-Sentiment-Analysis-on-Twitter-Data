rm(list=ls())                   # Clear workspace

#--------------------------------------------------------#
# Step 0 - Assign Library & define functions             #
#--------------------------------------------------------#

install.packages("text2vec")
install.packages("data.table")
install.packages("stringr")
install.packages("tm")
install.packages("tokenizers")
install.packages("slam")
install.packages("wordcloud")
install.packages("igraph")
install.packages("maptpx")
install.packages("RWeka")


library(text2vec)
library(data.table)
library(stringr)
library(tm)
library(RWeka)
library(tokenizers)
library(slam)
library(wordcloud)
library(igraph)
library(maptpx)

text.clean = function(x)                    # text data
{ require("tm")
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
  x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  return(x)
}

distill.cog = function(mat1, # input TCM ADJ MAT
                       title, # title for the graph
                       s,    # no. of central nodes
                       k1){  # max no. of connections  
  
  a = colSums(mat1) # collect colsums into a vector obj a
  b = order(-a)     # nice syntax for ordering vector in decr order  
  
  mat2 = mat1[b,b]  #
  
  diag(mat2) =  0
  
  ## +++ go row by row and find top k adjacencies +++ ##
  
  wc = NULL
  
  for (i1 in 1:s){ 
    thresh1 = mat2[i1,][order(-mat2[i1, ])[k1]]
    mat2[i1, mat2[i1,] < thresh1] = 0   # wow. didn't need 2 use () in the subset here.
    mat2[i1, mat2[i1,] > 0 ] = 1
    word = names(mat2[i1, mat2[i1,] > 0])
    mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
    wc = c(wc,word)
  } # i1 loop ends
  
  
  mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
  ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
  mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
  graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
  graph = simplify(graph) 
  V(graph)$color[1:s] = "green"
  V(graph)$color[(s+1):length(V(graph))] = "pink"
  
  graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ])
  
  plot(graph, 
       layout = layout.kamada.kawai, 
      main = title)

  } # func ends

#--------------------------------------------------------#
# Step 1 - Reading text data                             #
#--------------------------------------------------------#

search_terms = c('LasVegas','shootings', 'GuncontrolNow')
#search_terms = c('youthmovement','funday','peace')


file.cr = read.csv(paste0("C:/Users/Navya/Desktop/UIC/Fall 2017/IDS 566 Text Mining/Project/","LasVegas.csv"))
file.mi = read.csv(paste0("C:/Users/Navya/Desktop/UIC/Fall 2017/IDS 566 Text Mining/Project/","shootings.csv"))
file.lin = read.csv(paste0("C:/Users/Navya/Desktop/UIC/Fall 2017/IDS 566 Text Mining/Project/","GunControlNow.csv"))

file.cr = file.cr[!is.na(file.cr$text)|file.cr$text != '',]

file.mi = file.mi[!is.na(file.mi$text)|file.mi$text != '',]

file.lin = file.lin[!is.na(file.lin$text)|file.lin$text != '',]

n = min(nrow(file.cr),nrow(file.mi),nrow(file.lin))

data = data.frame(id = 1:n, text1 = file.cr$text[1:n],
                      text2 = file.mi$text[1:n],
                      text3 = file.lin$text[1:n],
                      stringsAsFactors = F)
data$text = paste(data$text1,data$text2,data$text3)

dim(data)

# Read Stopwords list

stpw = tm::stopwords('english')               # tm package stop word list; tokenizer package has the same name function

stopwords = unique(gsub("'"," ",stpw))  # final stop word lsit after removing punctuation

x  = text.clean(data$text)             # pre-process text corpus
x  =  removeWords(x,stopwords)            # removing stopwords created above
x  =  stripWhitespace(x)                  # removing white space

#--------------------------------------------------------#
####### Create DTM using text2vec package                #
#--------------------------------------------------------#

t1 = Sys.time()

tok_fun = word_tokenizer

it_0 = itoken( x,
               tokenizer = tok_fun,
               ids = data$id,
               progressbar = T)

vocab = create_vocabulary(it_0,
                          ngram = c(2L, 2L)
                         )

pruned_vocab = prune_vocabulary(vocab,
                                term_count_min = 10)

vectorizer = vocab_vectorizer(pruned_vocab)

dtm_0  = create_dtm(it_0, vectorizer)

# Sort bi-gram with decreasing order of freq
tsum = as.matrix(t(rollup(dtm_0, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
tsum = tsum[order(tsum, decreasing = T),]       #terms in decreasing order of freq
head(tsum)
tail(tsum)

# select Top 1000 bigrams to unigram
if (length(tsum) > 1000) {n = 1000} else {n = length(tsum)}
tsum = tsum[1:n]

#-------------------------------------------------------
# Code bi-grams as unigram in clean text corpus

text2 = x
text2 = paste("",text2,"")

pb <- txtProgressBar(min = 1, max = (length(tsum)), style = 3) ; i = 0

for (term in names(tsum)){
  i = i + 1
  focal.term = gsub("_", " ",term)        # in case dot was word-separator
  replacement.term = term
  text2 = gsub(paste("",focal.term,""),paste("",replacement.term,""), text2)
  setTxtProgressBar(pb, i)
}


it_m = itoken(text2,
              tokenizer = tok_fun,
              ids = data$id,
              progressbar = T)

vocab = create_vocabulary(it_m)

pruned_vocab = prune_vocabulary(vocab,
                                term_count_min = 1)

vectorizer = vocab_vectorizer(pruned_vocab)

dtm_m  = create_dtm(it_m, vectorizer)
dim(dtm_m)

dtm = as.DocumentTermMatrix(dtm_m, weighting = weightTf)

print(difftime(Sys.time(), t1, units = 'sec'))

# some basic clean-up ops
dim(dtm)

a0 = apply(dtm, 1, sum)   # apply sum operation to dtm's rows. i.e. get rowSum
  dtm = dtm[(a0 > 5),]    # retain only those rows with token rowSum >5, i.e. delete empty rows
  dim(dtm); rm(a0)        # delete a0 object

a0 = apply(dtm, 2, sum)   # use apply() to find colSUms this time
  dtm = dtm[, (a0 > 4)]     # retain only those terms that occurred > 4 times in the corpus
  dim(dtm); rm(a0)

# view summary wordlcoud
a0 = apply(dtm, 2, sum)     # colSum vector of dtm
  a0[1:5]                   # view what a0 obj is like
  a1 = order(as.vector(a0), decreasing = TRUE)     # vector of token locations
  a0 = a0[a1]     # a0 ordered asper token locations
  a0[1:5]         # view a0 now

windows() # opens new image window
wordcloud(names(a0), a0,     # invoke wordcloud() func. Use ?wordcloud for more info
          scale=c(4,1), 
          3, # min.freq 
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))
title(sub = "Quick Summary Wordcloud")

#------------------------------------------------------#
# Step 1a - Term Co-occurance Matrix                             #
#------------------------------------------------------#

pruned_vocab = prune_vocabulary(vocab,
                                term_count_min = 5)

vectorizer = vocab_vectorizer(pruned_vocab)
tcm = create_tcm(it_m, vectorizer)

tcm.mat = as.matrix(tcm)
adj.mat = tcm.mat + t(tcm.mat)

# how about a quick view of the distilled COG as well, now that we're here?
diag(adj.mat) = 0     # set diagonals of the adj matrix to zero --> node isn't its own neighor
a0 = order(apply(adj.mat, 2, sum), decreasing = T)
adj.mat = as.matrix(adj.mat[a0[1:50], a0[1:50]])

windows()
distill.cog(adj.mat, 'Distilled COG for full corpus',  10,  10)


#################################################
## --- Step 2: model based text analytics ------ ###
#################################################
# -- select optimal num of topics

## Bayes Factor model selection (should choose K or nearby)

K = 3     # overriding model fit criterion

# -- run topic model for selected K -- #
summary( simfit <- topics(dtm,  K=K, verb=2), nwrd = 12 )
rownames1 = gsub(" ", ".", rownames(simfit$theta));  rownames(simfit$theta) = rownames1;  


## what are the factor components of the factorized DTM?

dim(dtm)     # size of the orig input matrix

str(simfit)     # structure of the output obj

dim(simfit$theta)   # analogous to factor loadings
dim(simfit$omega)   # analogous to factor scores 

simfit$theta[1:5,]
simfit$omega[1:5,]


## Latent Dirichlet Allocation
install.packages("topicmodels")
library(topicmodels)
lda_dtm <- LDA(dtm, k = 4) #function to compute LDA
term <- terms(lda_dtm, 5) # first 5 terms of every topic
term
