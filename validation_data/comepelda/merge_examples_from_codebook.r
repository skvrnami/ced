
# The examples assume the three datasets are read in as objects:
# da (aggregate), dc (candidates), and dm (MEPs), e.g. using:

da <- read.csv(file="COMEPELDA_aggregate_v1.00.csv",
               header=TRUE, encoding = "UTF-8", na.strings=".")
dc <- read.csv(file="COMEPELDA_candidates_v1.00.csv",
               header=TRUE, encoding = "UTF-8", na.strings=".")
dm <- read.csv(file="COMEPELDA_meps_v1.00.csv",
               header=TRUE, encoding = "UTF-8", na.strings=".")



### Example 1:
# add info from Aggregate Data to Candidate Data
# here: absolute party votes (and number of candidates)

dana <- merge(dc,
			        da[,c("IDAD","pdAbs", "pdNofCandEl")],
              by = "IDAD",
              all.x=TRUE)
# Share of all party voters supporting the candidate:
dana$pvsupport <- dana$PrefVot/dana$pdAbs
summary(dana$pvsupport)
# Exclude instances when there is just one candidate:
summary(dana$pvsupport[dana$pdNofCandEl > 1])
# look at candidates with > 90% support:
View(dana[!is.na(dana$pvsupport) & dana$pvsupport > .9 & dana$pdNofCandEl > 1, c("cName","ElYear","IDpty","dName","Name","pdAbs","PrefVot","pvsupport")])

### Example 2:
# add info from Aggregate Data to MEP Data
# (here: vote share of party in district in previous election)

dana <- merge(dm,
			        da[,c("IDAD","pdShare")],
              by.x="IDADfocLag", by.y="IDAD",
              all.x=TRUE)
summary(dana$pdShare)

# (here: vote share of party in district in following election)

dana <- merge(dm, 
			        da[,c("IDAD","pdShare")],
              by.x="IDADfocLead", by.y="IDAD",
              all.x=TRUE)
summary(dana$pdShare)

### Example 3: 
# add info from Candidate Data to MEP Data
# here: list rank related to previous and following election

dana <- merge(dm, 
			        dc[,c("IDCD","ListRankSel")],
              by.x="IDCDfocLag", by.y="IDCD",
              all.x=TRUE)
names(dana)[names(dana)=="ListRankSel"] <- "ListRankSelLag"

dana <- merge(dana,
			        dc[,c("IDCD","ListRankSel")],
              by.x="IDCDfocLead", by.y="IDCD",
              all.x=TRUE)
names(dana)[names(dana)=="ListRankSel"] <- "ListRankSelLead"

summary(dana[, c("ListRankSelLag", "ListRankSelLead")])
# basic (!) comparison of list ranks over time
table(sign(dana$ListRankSelLead - dana$ListRankSelLag))
# this will be missing if parties don't rank lists
# observed for MEPs who have run in both the previous and the following election
# (this basic analysis ignores that MEPs may switch parties, lists (main vs successor), or districts)

### Example 4:
# add info from MEP Data to Aggregate Data
# here: relative frequency of replacing MEPs in the party during the previous term

library(dplyr)
tmp <- summarize(group_by(dm, EP, IDADfocLead),
                 Ninit = sum(EpisType %in% c("init elec","repl begin")), # N of MEPs after election
                 Nrepl = sum(EpisType == "repl sess")) # N of replacements


dana <- merge(da, 
			        tmp[tmp$Ninit > 0, c("IDADfocLead","Ninit","Nrepl")],
              by.x="IDAD", by.y="IDADfocLead",
              all.x=TRUE)

table(dana$Nrepl)
# distribution of replacement rates:
summary(dana$Nrepl/dana$Ninit)


### Example 5:
# add info from MEP Data to Candidate Data
# here: episode type related to outgoing term

dana <- merge(dc, 
			        dm[,c("IDMD","EpisType")],
              by.x="IDMDoutall", by.y="IDMD",
              all.x=TRUE)
			  
table(dana$EpisType, useNA = "ifany")
table(dana$EpisType[!is.na(dana$IDMDoutall)], useNA = "ifany")

# We could use IDMDoutrep instead of IDMDoutall to match this only for the district the MEP represented




