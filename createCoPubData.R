library(xml2)
library(dplyr)
library(RColorBrewer)
library(grDevices)
library(xlsx)
options(stringsAsFactors = FALSE)

authorList <- 'traingGrantAuthor.singleInitial.list'

# (!) Make sure to update reldate paramter:
# reldate: when reldate is set to an integer n, the search returns only those items that have a date specified by datetype within the last n days.

url <- 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&datetype=pdat&reldate=14600&RetMax=500&term='

authors <- read.table(authorList, sep = ';', strip.white = TRUE, header = FALSE, quote = '')

a <- t(combn(authors$V2, 2))
n <- 0       
r <- bind_rows(apply(a, 1, function(x){
         n <<- n + 1
         message(paste(x[1], '::', x[2], ' ', n, '/', nrow(a)))
         u <- paste0(url, '(', x[1], '[Author] OR ', x[1], '[Investigator]) AND (', x[2], '[Author] OR ', x[2], '[Investigator])')
        
         o <- as_list(read_xml(paste0(readLines(URLencode(u)), collapse = '\n')))
         Sys.sleep(1)
         data.frame(author1 = x[1], 
                    author2 = x[2], 
                    pubs    = unlist(o$eSearchResult$Count), 
                    pubIDs  = paste0(unlist(o$eSearchResult$IdList), collapse = ', '))
     }))

r$pubs <- as.integer(r$pubs)

# Write our an Excel spreadsheet and create a data image.
write.xlsx(r, file = 'coPubs.xlsx', col.names = TRUE, row.names = FALSE)
save.image(file = 'image.RData')

# Subset the data to include only authors with 1 or more co-publications
r <- subset(r, r$pubs > 0)
dplyr::select(r, -pubIDs)
write.csv(dplyr::select(r, -pubIDs), 'network.csv', row.names = FALSE, quote = FALSE)
