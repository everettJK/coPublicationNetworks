library(xml2)
library(dplyr)
options(stringsAsFactors = FALSE)

correctAuthorNames <- function(n){
  n <- unlist(strsplit(n, '\\s+'))
  n[2] <- toupper(n[2])
  o <- unlist(strsplit(n[1], ''))
  o[1] <- toupper(o[1])
  o[2:length(o)] <- tolower(o[2:length(o)])
  n[1] <- paste0(o, collapse = '')
  n <- paste(n, collapse = ' ')
  n <- sub('\\-\\S', paste0('-', toupper(stringr::str_match(n, '\\-(\\S)')[,2]), collapse = ''), n)
  n
}

authors <- trimws(scan('author.list', what = 'character', sep = '\n'))
authors <- sapply(authors, correctAuthorNames)

# Base URL to find publications in pubmed within the last 5 years (1825 days).
url <- 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&datetype=pdat&reldate=1825&RetMax=500&term='

r <- bind_rows(apply(t(combn(authors, 2)), 1, function(x){
         u <- paste0(url, x[1], '[Author]+AND+', x[2], '[Author]')
         o <- as_list(read_xml(paste0(readLines(URLencode(u)), collapse = '\n')))
         browser()
         data.frame(author1 = x[1], 
                    author2 = x[2], 
                    pubs    = unlist(o$eSearchResult$Count), 
                    pubIDs  = paste0(unlist(o$eSearchResult$IdList), collapse = ','))
     }))

save.image(file = 'image.RData')



o <- read.table('author.overrides', sep=',', header = FALSE)
invisible(apply(o, 1, function(x){
  i <- which(r$author1 == x['V1'] & r$author2 == x['V2'])
  if(length(i) > 0) r[i,]$pubs <<- as.integer(x['V3'])
  i <- which(r$author1 == x['V2'] & r$author2 == x['V1'])
  if(length(i) > 0)r[i,]$pubs <<- as.integer(x['V3'])
}))


r2 <- subset(r, r$pubs > 0)


departments <- read.table('author.departments', sep = '\t', quote = '', strip.white = TRUE, comment.char = '')
departments$V1 <- sapply(departments$V1, correctAuthorNames)
departments$V2 <- factor(departments$V2)
departments$V3 <- as.integer(departments$V2)
write.table(departments, sep = '\t', file = 'r.departments.tsv', col.names = FALSE, row.names = FALSE, quote = FALSE)

r2$edgeType <- 'coPub'
r2$pubs <- as.integer(r2$pubs)
write.table(r2[,c('author1', 'edgeType', 'author2', 'pubs')], file = 'r.tsv', sep = '\t', col.names = FALSE, row.names = FALSE, quote = FALSE)


