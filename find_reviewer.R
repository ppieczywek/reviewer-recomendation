
source("helper_functions.R")

# uploads data for pre-built reviewer data base
scores <- read_csv(file = "data//reviewer_scores.csv")
keywords <- read_csv(file = "data//reviewer_keywords.csv")

# transforms table to long format and calculates log probabilities of keywords
# for each reviewer
scores %>%
  select(-email) %>%
  pivot_longer(cols=W1:W80,
               names_to = "keyword_index",
               values_to = "keyword_count") %>%
  mutate(probability = log((keyword_count+1)/ total_count)) %>%
  select(-keyword_count, -total_count) -> probabilities

# transforms table to long format 
keywords %>%
  pivot_longer(cols=W1:W80,
               names_to = "keyword_index",
               values_to = "keyword") -> keywords

# combines keywords and probabilities into single table in long format
reviewer.table <- left_join(probabilities, 
                            keywords, 
                            by=c("name", "document", "keyword_index"))

rm(scores, keywords, probabilities)

# extracts keywords for a give publication
target.publication.path <- "data//Z4_P5_Pieczywek_CARP_2021.pdf"
target.publication.info <- extract_pdf_keywords(target.publication.path,
                                                80,
                                                to.exclud)


# compares reviewers and publication bags of words, sums the probabilities and
# sorts reviewers based on calculated scores
reviewer.table %>%
  group_by(name, document) %>%
  mutate(has.match = is.na(match(keyword, 
                                 target.publication.info$keywords))) %>%
  mutate(score = if_else(has.match,
                         log(1/9000000),
                         probability)) %>%
  arrange(desc(score)) %>%
  select(-probability, -keyword_index) %>%
  summarise(total.score = sum(score),
            matched.keywords = paste(keyword[which(has.match==FALSE)],
                                     collapse = ', '),
            email = email[1]) %>%
  ungroup() %>%
  arrange((desc(total.score))) %>%
  slice(1:10) -> reviewer.suggestions

# prints ten best suggestions of reviewers for a given publication
print(reviewer.suggestions)