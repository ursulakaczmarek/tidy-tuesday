library(tidytuesdayR)
library(tidyverse)
library(tm)
library(networkD3)

# load data
list2env(tt_load('2023-10-03'), globalenv())

# function to remove nonwords + stopwords from funding description text
remove_words <- function(string, words) {
  cleaned <- tolower(gsub(string, pattern = "[0-9]+|[[:punct:]]", replacement = " "))
  stemmed <- stemDocument(cleaned, language = "english")
  description <- strsplit(stemmed, " ", fixed = TRUE)
  vapply(description, function(x){paste(x[!tolower(x) %in% words], collapse = " ")}, character(1))
}

words_to_remove <- c(stopwords(), "apply", "applic", "also", "address", "assist", "activ", "appropri",
  "approach", "avail", "advanc", "announc", "award", "center", "collabor", "develop", "department", 
  "element", "encourag", "fiscal", "focus", "fund", "grant", "high", "guideline", "initi",
  "implement", "institut", "includ", "impact", "inform", "innov", "improv", "investig", "individu",
  "manag", "nation", "opportun", "office", "organ", "phase",  "provid", "program", "purpos", 
  "propos", "project", "process", "resourc", "region", "request", "relief", "requir", "relat", 
  "submit", "state", "studi","specif", "system", "solicit", "support", "united", "within", "year", "")

# get agency nodes and word counts of grant descriptions
# extract first three letters to group program offices within whole agencies
agency <- grants %>% 
  filter(!is.na(agency_code)) %>% 
  mutate(opportunity_id = opportunity_id,
          agency = substr(agency_code, 1, 3)) %>% 
  inner_join(grant_opportunity_details %>% select(opportunity_id, description, estimated_total_program_funding), by = "opportunity_id") %>% 
  transmute(agency = agency,
            estimated_total_program_funding = estimated_total_program_funding,
            subject = remove_words(description, words_to_remove)) %>% 
  separate_longer_delim(subject, delim = " ") %>% 
  group_by(subject) %>% 
  mutate(total_subject_mentions = n(),
         total_subject_funding = sum(estimated_total_program_funding, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(nchar(subject) > 4 & dense_rank(-total_subject_mentions) <= 50) %>% 
  select(-estimated_total_program_funding) %>% 
  distinct()

# build node and edge tibbles convertible to graph tables
nodes <- list(unique(agency["agency"]), unique(agency["subject"])) %>% 
  bind_rows() %>% 
  left_join(agency %>% select(c(subject, total_subject_funding, total_subject_mentions)) %>% distinct(), by = "subject") %>% 
  transmute(nodes = coalesce(agency, subject),
    total_subject_funding = sqrt(total_subject_funding/10^5), 
    total_subject_mentions = total_subject_mentions) %>% 
  rowid_to_column("id") %>% 
  mutate(id = id - 1) #networkD3 ids start at 0
  
edges <- agency %>% 
  left_join(nodes %>% select(id, nodes), by = c("agency" = "nodes")) %>% 
  rename(to = id)

edges <- edges %>% 
  left_join(nodes %>% select(id, nodes), by = c("subject" = "nodes")) %>% 
  rename(from = id)

nodes$group <- if_else(is.na(nodes$total_subject_mentions), "agency", "subject")

forceNetwork(Links = edges, Nodes = nodes,
             Source = "from", Target = "to", 
             NodeID = "nodes", Group = "group",
             Value = "total_subject_mentions", 
             Nodesize = "total_subject_funding",
             colourScale = JS('d3.scaleOrdinal().domain(["subject", "agency"]).range(["#009ec5", "#f39844"]);'),
             linkDistance = 5, 
             charge = -1500,
             linkWidth = JS("function(d) { return Math.sqrt(d.value*0.0001); }"),
             fontFamily = "sans-serif",
             fontSize = 16, 
             opacity = 0.7, 
             opacityNoHover = 1,
             zoom = TRUE)

  