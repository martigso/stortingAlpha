rm(list=ls())

library(dplyr);library(ministersNor);library(XML);library(RCurl);library(gsubfn);library(stortingAlpha)

url9697 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Skriftlige-sporsmal-og-svar/?pid=1996-97&qsqid=all&page=1#list"
written9697 <- parl.questions(url9697, 9)
written9697$session <- "1996-1997"

url9798 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Skriftlige-sporsmal-og-svar/?pid=1997-98&qsqid=all&page=1#list"
written9798 <- parl.questions(url9798, 15)
written9798$session <- "1997-1998"

url9899 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Skriftlige-sporsmal-og-svar/?pid=1998-99&qsqid=all&page=1#list"
written9899 <- parl.questions(url9899, 21)
written9899$session <- "1998-1999"

url9900 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Skriftlige-sporsmal-og-svar/?pid=1999-2000&qsqid=all&page=1#list"
written9900 <- parl.questions(url9900, 25)
written9900$session <- "1999-2000"

url0001 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Skriftlige-sporsmal-og-svar/?pid=2000-2001&qsqid=all&page=1#list"
written0001 <- parl.questions(url0001, 28)
written0001$session <- "2000-2001"

url0102 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Skriftlige-sporsmal-og-svar/?pid=2001-2002&qsqid=all&page=1#list"
written0102 <- parl.questions(url0102, 30)
written0102$session <- "2001-2002"

url0203 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Skriftlige-sporsmal-og-svar/?pid=2002-2003&qsqid=all&page=1#list"
written0203 <- parl.questions(url0203, 40)
written0203$session <- "2002-2003"

url0304 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Skriftlige-sporsmal-og-svar/?pid=2003-2004&qsqid=all&page=1#list"
written0304 <- parl.questions(url0304, 49)
written0304$session <- "2003-2004"

url0405 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Skriftlige-sporsmal-og-svar/?pid=2004-2005&qsqid=all&page=1#list"
written0405 <- parl.questions(url0405, 50)
written0405$session <- "2004-2005"

url0506 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Skriftlige-sporsmal-og-svar/?pid=2005-2006&qsqid=all&page=1#list"
written0506 <- parl.questions(url0506, 63)
written0506$session <- "2005-2006"

url0607 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Skriftlige-sporsmal-og-svar/?pid=2006-2007&qsqid=all&page=1#list"
written0607 <- parl.questions(url0607, 71)
written0607$session <- "2006-2007"

url0708 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Skriftlige-sporsmal-og-svar/?pid=2007-2008&qsqid=all&page=1#list"
written0708 <- parl.questions(url0708, 81)
written0708$session <- "2007-2008"

url0809 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Skriftlige-sporsmal-og-svar/?pid=2008-2009&qsqid=all&page=1#list"
written0809 <- parl.questions(url0809, 81)
written0809$session <- "2008-2009"

url0910 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Skriftlige-sporsmal-og-svar/?pid=2009-2010&qsqid=all&page=1#list"
written0910 <- parl.questions(url0910, 91)
written0910$session <- "2009-2010"

url1011 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Skriftlige-sporsmal-og-svar/?pid=2010-2011&qsqid=all&page=1#list"
written1011 <- parl.questions(url1011, 99)
written1011$session <- "2010-2011"

url1112 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Skriftlige-sporsmal-og-svar/?pid=2011-2012&qsqid=all&page=1#list"
written1112 <- parl.questions(url1112, 102)
written1112$session <- "2011-2012"

url1213 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Skriftlige-sporsmal-og-svar/?pid=2012-2013&qsqid=all&page=1#list"
written1213 <- parl.questions(url1213, 87)
written1213$session <- "2012-2013"

url1314 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Skriftlige-sporsmal-og-svar/?pid=2013-2014&qsqid=all&page=1#list"
written1314 <- parl.questions(url1314, 64)
written1314$session <- "2013-2014"

url1415 <- "https://www.stortinget.no/no/Saker-og-publikasjoner/Sporsmal/Skriftlige-sporsmal-og-svar/?pid=2014-2015&qsqid=all&page=1#list"
written1415 <- parl.questions(url1415, 63)
written1415$session <- "2014-2015"

writtenAll<-data.frame(rbind(written1415, written1314, written1213, written1112, written1011, written0910, written0809,
                            written0708, written0607, written0506, written0405, written0304, written0203, written0102,
                            written0001, written9900, written9899, written9798, written9697))

rm(written1415, written1314, written1213, written1112, written1011, written0910, written0809, written0708, written0607,
   written0506, written0405, written0304, written0203, written0102, written0001, written9900, written9899, written9798, written9697,
   url1415, url1314, url1213, url1112, url1011, url0910, url0809, url0708, url0607, url0506, url0405, url0304, url0203,
   url0102, url0001, url9900, url9899, url9798, url9697)


#save(writtenAll, file="C:\\Users\\Martin\\Desktop\\writtenAll.RData")



rm(nPages, i, tempAnswBy, tempDate, typeString, url)
