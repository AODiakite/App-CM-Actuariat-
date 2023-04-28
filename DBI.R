library(dplyr)
library(dbplyr)
DBP = DBP$as_data_frame()
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbWriteTable(conn = con,name =  "DBP",value =  DBP)
dbp = tbl(con,"DBP")
dbp %>%
  select(MNT_PAIE) %>%
  mutate(MNT_PAIE = gsub(",","\\.",MNT_PAIE) )
