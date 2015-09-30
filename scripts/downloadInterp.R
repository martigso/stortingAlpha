rm(list=ls())

library(dplyr);library(ministersNor);library(XML);library(RCurl);library(gsubfn);library(stortingAlpha)

url9697 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Interpellasjoner/?pid=1996-97&qsqid=all&page=1#list"
interp9697 <- parl.questions(url9697, 3)
interp9697$session <- "1996-1997"

url9798 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Interpellasjoner/?pid=1997-98&qsqid=all&page=1#list"
interp9798 <- parl.questions(url9798, 3)
interp9798$session <- "1997-1998"

url9899 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Interpellasjoner/?pid=1998-99&qsqid=all&page=1#list"
interp9899 <- parl.questions(url9899, 3)
interp9899$session <- "1998-1999"

url9900 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Interpellasjoner/?pid=1999-2000&qsqid=all&page=1#list"
interp9900 <- parl.questions(url9900, 3)
interp9900$session <- "1999-2000"

url0001 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Interpellasjoner/?pid=2000-2001&qsqid=all&page=1#list"
interp0001 <- parl.questions(url0001, 2)
interp0001$session <- "2000-2001"

url0102 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Interpellasjoner/?pid=2001-2002&qsqid=all&page=1#list"
interp0102 <- parl.questions(url0102, 2)
interp0102$session <- "2001-2002"

url0203 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Interpellasjoner/?pid=2002-2003&qsqid=all&page=1#list"
interp0203 <- parl.questions(url0203, 2)
interp0203$session <- "2002-2003"

url0304 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Interpellasjoner/?pid=2003-2004&qsqid=all&page=1#list"
interp0304 <- parl.questions(url0304, 3)
interp0304$session <- "2003-2004"

url0405 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Interpellasjoner/?pid=2004-2005&qsqid=all&page=1#list"
interp0405 <- parl.questions(url0405, 2)
interp0405$session <- "2004-2005"

url0506 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Interpellasjoner/?pid=2005-2006&qsqid=all&page=1#list"
interp0506 <- parl.questions(url0506, 3)
interp0506$session <- "2005-2006"

url0607 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Interpellasjoner/?pid=2006-2007&qsqid=all&page=1#list"
interp0607 <- parl.questions(url0607, 4)
interp0607$session <- "2006-2007"

url0708 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Interpellasjoner/?pid=2007-2008&qsqid=all&page=1#list"
interp0708 <- parl.questions(url0708, 4)
interp0708$session <- "2007-2008"

url0809 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Interpellasjoner/?pid=2008-2009&qsqid=all&page=1#list"
interp0809 <- parl.questions(url0809, 4)
interp0809$session <- "2008-2009"

url0910 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Interpellasjoner/?pid=2009-2010&qsqid=all&page=1#list"
interp0910 <- parl.questions(url0910, 5)
interp0910$session <- "2009-2010"

url1011 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Interpellasjoner/?pid=2010-2011&qsqid=all&page=1#list"
interp1011 <- parl.questions(url1011, 7)
interp1011$session <- "2010-2011"

url1112 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Interpellasjoner/?pid=2011-2012&qsqid=all&page=1#list"
interp1112 <- parl.questions(url1112, 5)
interp1112$session <- "2011-2012"

url1213 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Interpellasjoner/?pid=2012-2013&qsqid=all&page=1#list"
interp1213 <- parl.questions(url1213, 5)
interp1213$session <- "2012-2013"

url1314 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Interpellasjoner/?pid=2013-2014&qsqid=all&page=1#list"
interp1314 <- parl.questions(url1314, 4)
interp1314$session <- "2013-2014"

url1415 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Interpellasjoner/?pid=2014-2015&qsqid=all&page=1#list"
interp1415 <- parl.questions(url1415, 4)
interp1415$session <- "2014-2015"

interpAll<-data.frame(rbind(interp1415, interp1314, interp1213, interp1112, interp1011, interp0910, interp0809,
                            interp0708, interp0607, interp0506, interp0405, interp0304, interp0203, interp0102,
                            interp0001, interp9900, interp9899, interp9798, interp9697))

rm(interp1415, interp1314, interp1213, interp1112, interp1011, interp0910, interp0809, interp0708, interp0607,
   interp0506, interp0405, interp0304, interp0203, interp0102, interp0001, interp9900, interp9899, interp9798, interp9697,
   url1415, url1314, url1213, url1112, url1011, url0910, url0809, url0708, url0607, url0506, url0405, url0304, url0203,
   url0102, url0001, url9900, url9899, url9798, url9697)


#save(interpAll, file="C:\\Users\\Martin\\Desktop\\interpAll.RData")
