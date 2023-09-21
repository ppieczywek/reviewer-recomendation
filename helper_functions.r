if (require(pdftools) == FALSE){
  install.packages(pdftools)
}

if (require(tidyverse) == FALSE){
  install.packages(tidyverse)  
}

if (require(tm) == FALSE){
  install.packages(tm)
}

# set of irrelevant words which have to be excluded from a bag of words
to.exclud <- c("abil", "abstract", "accord", "acid", "acknowledg", "activ",
               "addit", "adjust", "advanc", "affect", "agreement",  "aim", "allow", "almost",
               "along", "also", "altern", "although", "among", "amount", "andor",
               "anim", "appear", "appli", "applic", "approach", "assess", "author",
               "avail", "basi", "behavior", "behaviour", "better", "brazil", "can",
               "capabl", "carri", "case", "caus", "chang", "charact", "characterist",
               "close", "collect", "combin", "common", "compar", "comparison", "complet",
               "compon", "conclud", "conclus", "condit", "conduct", "confirm",
               "consequ", "consid", "consist", "constant", "consumpt", "contain",
               "content", "continu", "contribut", "control", "correspond", "data",
               "day", "decrea", "depend", "describ", "detect", "determin", "develop",
               "discuss", "disper", "distribut", "doubl", "dri", "due", "effect", "effici",
               "either", "enabl", "equip", "european", "even", "exampl", "except", "exhibit",
               "expect", "experi", "explain", "express", "extend", "fig", "figur", "final",
               "first", "follow", "found", "general", "generat", "give", "given", "good",
               "great", "greater", "high", "higher", "highest", "howev", "immedi", "impact",
               "import", "inc", "includ", "incorpor", "increa", "index", "indic", "individu",
               "industri", "influenc", "ingredi", "institut", "instrument", "inten",
               "introduct", "investig", "japan", "journal", "keyword", "known", "larg",
               "lead", "least", "length", "less", "letter", "level", "like", "limit",
               "literatur", "loss", "lost", "loui", "low", "lower", "lowest", "ltd",
               "made", "magnitud", "main", "major", "make", "mani", "manufactur", "maximum",
               "may", "method", "methodolog", "min", "miner", "minut", "moreov", "move",
               "necessari", "new", "number", "numer", "observ", "obtain", "occur",
               "one", "over", "plot", "recent", "red", "regard", "relev", "remain", "remov",
               "research", "result", "review", "roo", "room", "second", "section", "seem",
               "small", "smaller", "suggest", "support", "switzerland", "take", "taken",
               "techniqu", "technolog", "treatment", "understand", "usa", "use", "usual",
               "whole", "wide", "will", "within", "without", "show", "total", "tabl",
               "studi", "shown", "amend", "analysi", "analyz", "averag", "decreas", "equal",
               "five", "gradual", "imag", "improv", "increas", "larger", "materi", "best",
               "experiment", "mean", "measur", "rmse", "sampl", "sci", "signific", "softwar",
               "curv", "differ", "int", "simultan", "percentag", "concentr", "map", "normal",
               "thus", "appeal", "end", "anoth", "refer", "sort", "three", "threshold", "test",
               "hour", "feasibl", "correl", "gain", "uptak", "begin", "grade", "predict",
               "fraction", "profess", "light", "cost", "jap", "knowledg", "train", "equat",
               "attribut", "batch", "khz", "design", "mbar", "ambient", "enhanc", "compound",
               "peak", "perfor", "project", "medium", "academia", "educ", "iso", "variabl",
               "steril", "reject", "treat", "scienc", "student", "coeffici", "refin",
               "specif", "countri", "concept", "turkish", "offer", "benefit", "score",
               "chile", "abl", "question", "respect", "untreat", "answer", "etc", "subunit",
               "detach", "attach", "inform", "address", "defin", "must", "therefor", "requir",
               "start", "find", "need", "approxim", "achiev", "chosen", "becom", "accur",
               "adequ", "the", "despit", "ensur", "academ", "campus", "two", "four",
               "cite", "tend", "throughout", "belong", "proper", "athen", "collabor",
               "universidad", "cambridg", "valu", "report" )

#' Extracts authors emails for a given pdf publication.
#' 
#' @param pdf.path A string with a path to pdf publication file.
#' @returns A vector with authors emails.
#' 
get_pdf_emails <- function(pdf.path) {
  data <- pdf_data(pdf.path) %>%
    bind_rows() %>%
    mutate(is.email = str_match(text,
                                pattern = "^[[:alnum:].-_]+@[[:alnum:].-]+$")) %>%
    drop_na() %>%
    select(text) -> e.mail
  result <- as.vector(e.mail$text)
}

#' Extracts authors names for a given pdf publication.
#' 
#' @param pdf.path A string with a path to pdf publication file.
#' @returns A vector with authors names.
#' 
get_pdf_authors <- function(pdf.path) {
  info <- pdf_info(pdf.path)
  meta <- unlist(strsplit(info$metadata, "\n"))
  limits <- which(!is.na(str_match(meta,
                                   pattern = "(.*)dc:creator>(.*)")[,1]))
  if (length(limits)) {
    authors.section <- meta[seq(from=limits[1],
                                to = limits[2])]
    authors <- str_match(authors.section,
                         pattern = "<rdf:li>(.*?)</rdf:li>")[,2]
    result <- authors[!is.na(authors)]
  } else {
    result <- c()
  }
}

#' Extracts authors, emails, keywords, keyword counts, keyword probabilities and
#' word count for a given pdf publication.
#' 
#' @param pdf.path A string with a path to pdf publication file.
#' @param n.keywords A number of keywords to extract.
#' @param excluded Vector of words to exlude from bag of words.
#' @returns A list with authors, emails, keywords, keyword counts, keyword probabilities and
#' word count.
extract_pdf_keywords <- function(pdf.path, n.keywords = 80, excluded = c()) {
  
  corpus <- Corpus(URISource(pdf.path), 
                   readerControl = list(reader = readPDF))
  corpus <- tm_map(corpus, removePunctuation, ucp = TRUE)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  corpus <- tm_map(corpus, stemDocument, lazy=TRUE)
  corpus <- tm_map(corpus, removeNumbers, lazy=TRUE)
  
  dtm <- TermDocumentMatrix(corpus)
  total.count <- sum(as.matrix(dtm))
  
  if (length(excluded) > 1) {
    corpus <- tm_map(corpus, removeWords, excluded)
  }
  
  dtm <- TermDocumentMatrix(corpus)
  keyword.frequencies <- findFreqTerms(dtm, lowfreq = 1, highfreq = Inf)
  keyword.frequencies <- as.matrix(dtm[keyword.frequencies,])
  
  keyword.frequencies <- keyword.frequencies[order(keyword.frequencies[,1],
                                                   decreasing=TRUE), ]
  
  emails <- get_pdf_emails(pdf.path)
  authors <- get_pdf_authors(pdf.path)
  
  result <- list(keywords = names(keyword.frequencies)[1:n.keywords],
                 keywords.counts = keyword.frequencies[1:n.keywords],
                 keywords.prob = log((keyword.frequencies[1:n.keywords] + 1) / total.count),
                 total.count = total.count,
                 emails = emails,
                 authors = authors)
}


