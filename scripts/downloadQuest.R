
rm(list=ls())
library(dplyr);library(ministersNor);library(XML);library(RCurl);library(gsubfn);library(stortingAlpha)

url9697 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Sporretimesporsmal/?pid=1996-97&qtid=all&qsqid=all&page=1#list"
quest9697 <- parl.questions(url9697, nPages = 22)
quest9697$session <- "1996-1997"

url9798 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Sporretimesporsmal/?pid=1997-98&qtid=all&qsqid=all&page=1#list"
quest9798 <- parl.questions(url9798, nPages = 46)
quest9798$session <- "1997-1998"

url9899 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Sporretimesporsmal/?pid=1998-99&qtid=all&qsqid=all&page=1#list"
quest9899 <- parl.questions(url9899, nPages = 43)
quest9899$session <- "1998-1999"

url9900 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Sporretimesporsmal/?pid=1999-2000&qtid=all&qsqid=all&page=1#list"
quest9900 <- parl.questions(url9900, nPages = 42)
quest9900$session <- "1999-2000"

url0001 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Sporretimesporsmal/?pid=2000-2001&qtid=all&qsqid=all&page=1#list"
quest0001 <- parl.questions(url0001, nPages = 38)
quest0001$session <- "2000-2001"

url0102 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Sporretimesporsmal/?pid=2001-2002&qtid=all&qsqid=all&page=1#list"
quest0102 <- parl.questions(url0102, nPages = 34)
quest0102$session <- "2001-2002"

url0203 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Sporretimesporsmal/?pid=2002-2003&qtid=all&qsqid=all&page=1#list"
quest0203 <- parl.questions(url0203, nPages = 32)
quest0203$session <- "2002-2003"

url0304 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Sporretimesporsmal/?pid=2003-2004&qtid=all&qsqid=all&page=1#list"
quest0304 <- parl.questions(url0304, nPages = 27)
quest0304$session <- "2003-2004"

url0405 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Sporretimesporsmal/?pid=2004-2005&qtid=all&qsqid=all&page=1#list"
quest0405 <- parl.questions(url0405, nPages = 21)
quest0405$session <- "2004-2005"

url0506 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Sporretimesporsmal/?pid=2005-2006&qtid=all&qsqid=all&page=1#list"
quest0506 <- parl.questions(url0506, nPages = 19)
quest0506$session <- "2005-2006"

url0607 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Sporretimesporsmal/?pid=2006-2007&qtid=all&qsqid=all&page=1#list"
quest0607 <- parl.questions(url0607, nPages = 19)
quest0607$session <- "2006-2007"

url0708 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Sporretimesporsmal/?pid=2007-2008&qtid=all&qsqid=all&page=1#list"
quest0708 <- parl.questions(url0708, nPages = 23)
quest0708$session <- "2007-2008"

url0809 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Sporretimesporsmal/?pid=2008-2009&qtid=all&qsqid=all&page=1#list"
quest0809 <- parl.questions(url0809, nPages = 19)
quest0809$session <- "2008-2009"

url0910 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Sporretimesporsmal/?pid=2009-2010&qtid=all&qsqid=all&page=1#list"
quest0910 <- parl.questions(url0910, nPages = 24)
quest0910$session <- "2009-2010"

url1011 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Sporretimesporsmal/?pid=2010-2011&qtid=all&qsqid=all&page=1#list"
quest1011 <- parl.questions(url1011, nPages = 23)
quest1011$session <- "2010-2011"

url1112 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Sporretimesporsmal/?pid=2011-2012&qtid=all&qsqid=all&page=1#list"
quest1112 <- parl.questions(url1112, nPages = 17)
quest1112$session <- "2011-2012"

url1213 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Sporretimesporsmal/?pid=2012-2013&qtid=all&qsqid=all&page=1#list"
quest1213 <- parl.questions(url1213, nPages = 16)
quest1213$session <- "2012-2013"

url1314 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Sporretimesporsmal/?pid=2013-2014&qtid=all&qsqid=all&page=1#list"
quest1314 <- parl.questions(url1314, nPages = 21)
quest1314$session <- "2013-2014"

url1415 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Sporretimesporsmal/?pid=2014-2015&qtid=all&qsqid=all&page=1#list"
quest1415 <- parl.questions(url1415, nPages = 19)
quest1415$session <- "2014-2015"


questAll<-data.frame(rbind(quest1415, quest1314, quest1213, quest1112, quest1011, quest0910, quest0809, quest0708, quest0607, quest0506,
                           quest0405, quest0304, quest0203, quest0102, quest0001, quest9900, quest9899, quest9798, quest9697))

rm(quest1415, quest1314, quest1213, quest1112, quest1011, quest0910, quest0809, quest0708, quest0607, quest0506,
   quest0405, quest0304, quest0203, quest0102, quest0001, quest9900, quest9899, quest9798, quest9697,
   url1415, url1314, url1213, url1112, url1011, url0910, url0809, url0708, url0607, url0506,
   url0405, url0304, url0203, url0102, url0001, url9900, url9899, url9798, url9697)

#save(questAll, file="C:\\Users\\Martin\\Desktop\\questAll.RData")
