#Data Extraction - Scraping text in arabic from wikipedia and creating an external corpus  
library(tidyverse)            # Data Manipulation
library(WikipediR)            # Wikipedia Queries
library(rvest)                # Web Scraping
library(tm) #for extra whitespace removal,punctuation etc.
library(NLP) #supporting tm library
library(qdapRegex)

setwd("C:\\Users\\Dana Oz\\Desktop\\Projects in R\\Cyber NLP Analysis")
library(readxl)
dict<-read_excel("Top-50000-Arabic-Words_clean.xlsx",col_types = c("text", "text"))#remove stopwords??
terms = unlist(dict[,1]) #terms= vector of terms to extract  
names(terms)<-NULL
terms<-mgsub::mgsub(terms, " ","_") #replace whitespace with "_" to all terms  
textlist<-list(NULL)

# Returns text from wikipedia, given a search term
for ( i in 1:length(terms)){
web_address <- paste0("https://ar.wikipedia.org/wiki/",terms[i])
tryCatch(
{page_html <- read_html(web_address)
page_paragraphs <- html_nodes(page_html,"p")
page_text <- paste(html_text(page_paragraphs), sep = '', collapse = '')
# text cleaning according to wikipedia's outcome 
page_text <- gsub("\n", " ", page_text)       # remove the existing line breaks
page_text <- gsub("\\.", "\\n", page_text)   # add line breaks
page_text <- gsub("\\[\\d\\]"," ", page_text) # remove citations
page_text <- gsub("[[:punct:]]"," ", page_text) # remove punctuation
page_text <- page_text %>% tolower()
page_text<-removeNumbers(page_text)
page_text<-stripWhitespace(page_text)
page_text<-rm_non_words(page_text)
page_text<-removePunctuation(page_text) ##notice this if adding the math cleaning part
textlist[i] = page_text}, #corpus: list, each element a wikipedia page clean text
error= function(cond) {
  message(paste("WEB does not seem to exist:", i))
})
}

textlist<-textlist[-which(sapply(textlist, is.null))] #unlist to create one big text 
corpus<-as.vector(do.call(rbind, textlist))
corpus<-corpus[-which(corpus=="")]
#remove english characters
eng_chars<-"qwertyuiopasdfghjklzxcvbnm"
eng_chars<-unlist(strsplit(eng_chars,"")) #create a pattern vector to remove (abc)
corpus<-textclean::mgsub(corpus, eng_chars, "")
orig_local<-Sys.getlocale(category = "LC_CTYPE") #"English_Israel.1252"
Sys.setlocale("LC_CTYPE", "arabic" )
write.csv(corpus, file = 'Wikipedia_Arabic_Corpus.txt', fileEncoding = "UTF-8", row.names = F) #saving as text, csv causes problem with arabic encoding
Sys.setlocale("LC_CTYPE", orig_local )
