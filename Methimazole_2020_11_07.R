# ##서울대병원 메티마졸 - 오서진
# 

# 필요한 패키지 설치하기 -------------------------------------------------------------------
wants <- c("DatabaseConnector", "moonBook", "SqlRender", "RPostgreSQL", 
           "lubridate", "RDocumentation", "tidyverse", "lubridate", "readxl", 
           "survival", "survminer", "dplyr", "ggplot2", "ggthemes", "pander")

has <- wants %in% rownames(installed.packages())

if(any(!has)) install.packages(wants[!has])

invisible(lapply(wants, library, character.only = TRUE))

rm(has, wants)


# DB 접속 세팅 ----------------------------------------------------------------

rm(list = ls())
mydbtype= "postgresql"      # db type: "postgresql", "oracle", "pdw", "redshift", "impala", "netezza", "bigquery", "redshift", "sqlite", "mssql", "sql server"
mydbname = "postgres"      # database name
myschemaname = ""      # schema name
myhost = ""      # db host
myport = ""               # db port
myuserid = "" # db id
mypassword = ""         #password
myvocabschemaname=""   # OMOP CDM vocabulary schema name

drv = dbDriver("PostgreSQL") # "MySQL", "PostgreSQL", "RSQLite"
con = dbConnect(drv,
                dbname= mydbname,
                host= myhost,
                port= myport,
                user= myuserid,
                password= mypassword)



## drug ##########################################
# 각 기관에서 각 약물에 해당하는 코드로 변경하기
methimazole <- c(1504672)
carbimazole <- c(19107525, 19040608, 19006936)
total <- c(1504672, 19107525, 19040608, 19006936)

# ATC Code
#class1a <- c('C02AB', 'A07EC03', 'C10AB02', 'N03AX24', 'H03BB01', 'N02AA08', 'L01BC01', 'L01BC01', 'J04BA02', 'C09AA02',
'C03CA01', 'J04AC01', 'A07EC02', 'J01XD01', 'P01CX01', 'C10AA03', 'C01BA02', 'A11HA02', 'C10AA01',
'P01CB02', 'J01EC01', 'M01AB02', 'D06AA04', 'N03AG01')

#class1b <- c('L01XX14', 'C01BD01', 'L04AX01', 'G03GB02', 'S01BA01', 'L01AA06', 'J05AF05', 'C09CA01', 'G03AC02',
'G03CA01', 'L01BB02', 'H03BB02', 'J05AE04', 'G03AC01', 'A02BC01', 'G03CA57', 'J01EE01')

#class2 <- c('N02BE01', 'C03AA04', 'N05AH02', 'J05AF02', 'D10AF02', 'G03C', 'L01XX02', 'L01XX24', 'N01AX10', 'L02BA01')

## measurement ###################################
# 각 기관에서 각 검사에 해당하는 코드로 변경하기
amylase <- c(3016771)
lipase <- c(3004905)
ast <- c(3042781)
alt <- c(3006923)
weight <- c(3013762)
height <- c(3015514)

lab<-c(amylase, lipase, ast, alt, total_bilirubin)

# 코호트 생성 ------------------------------------------------------------------
###메티마졸, 카르비마졸 복용한 환자 리스트 : 아이디, 최초투약일 
### 최초 메티마졸 투약일 이전 최소  6개월이상 관찰한 사람을 대상으로 함

# -- METHIMAZOLE AND CARBIMAZOLE Drug Exposure
"
SELECT * 
FROM @schema.drug_exposure
WHERE drug_concept_id in (@total)
ORDER BY person_id, drug_exposure_start_date, drug_exposure_end_date;
" -> sql

mmzcar <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname, total = total))
head(mmzcar)
tail(mmzcar)


# CDM 상 첫 mmz 처방일 (서울대병원 기준 -  "2004-10-14")
"
SELECT min(drug_exposure_start_date)
FROM @schema.drug_exposure
WHERE drug_concept_id in (@total)
" -> sql
firstdate <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname, total = total))
firstdate[1,]
class(firstdate[1,])

# 첫 6개월 간 처방된 기록이 있는 사람 제외 (washout period)
# cohort 시작일 : 첫 mmz 처방일 + 6개월 (서울대병원 기준 - "2005-04-14")
indexdate <- firstdate[1,] + months(6)
indexdate

mmzcar_x <- mmzcar %>% filter(mmzcar$drug_exposure_start_date < indexdate)
mmzcar_o <- mmzcar %>% filter(mmzcar$drug_exposure_start_date >= indexdate)

# 버릴 데이터 (서울대병원 기준 - 4604건)
nrow(mmzcar_x)

# 킵할 데이터 (서울대병원 10/29 기준 - 145479건)
nrow(mmzcar_o)

# 킵할 데이터 중에 버릴 건 (첫 6개월 간 처방된 기록이 있는 사람 버림) : (서울대병원 10/29 기준 - 28996 건)
# 킵할 데이터 중 킵할 건 : (서울대병원 10/29 기준 - 116483 건)

throw = 0
keep = 0
for ( ii in 1:(nrow(mmzcar_o)) ) {
  if (mmzcar_o[ii,]$person_id %in% mmzcar_x$person_id) {
    throw = throw + 1}
  else {keep = keep + 1}
}
throw
keep

mmzcar_keep <- mmzcar_o[FALSE,]
mmzcar_keep

# 앞 6개월 버릴 데이터 짤라낸 데이터 : (서울대병원 10/29 기준 - 116483 건)

mmzcar_keep <- mmzcar_o[!mmzcar_o$person_id %in% mmzcar_x$person_id,]
nrow(mmzcar_keep)

# cohort 생성 - episode numbering 해주기
# episode 1 numbering code

mmzcar_keep$episode = rep(0,nrow(mmzcar_keep))
mmzcar_keep[1,]$episode = as.numeric("1")

# episode numbering code
# washout period = 56 days

for ( ii in 2:(nrow(mmzcar_keep)) ) {
  if ((mmzcar_keep[ii,]$person_id == mmzcar_keep[ii-1,]$person_id) &
      as.Date(mmzcar_keep[ii,]$drug_exposure_start_date) <= as.Date(mmzcar_keep[ii-1,]$drug_exposure_end_date) + 56) {
    mmzcar_keep[ii,]$episode = mmzcar_keep[ii-1,]$episode }
  else {mmzcar_keep[ii,]$episode = mmzcar_keep[ii-1,]$episode + 1}
}


ep <- mmzcar_keep[nrow(mmzcar_keep),]$episode
ep
# total episode (서울대병원 10/29 기준 - 15180 episodes)

#--------------------------------------------------------------------------

mmzcar_blank <- data.frame(drug_exposure_id = integer(),
                           person_id = integer(),
                           drug_concept_id = integer(),
                           episode = integer(),
                           drug_exposure_start_date = as.Date(character()),
                           drug_exposure_end_date = as.Date(character()),
                           total_quantity = numeric(),
                           stringsAsFactors = FALSE)


for (i in 1: 15180) {
  temp <- mmzcar_keep %>% filter(episode == i)
  mmzcar_blank[i,]$episode = i
  mmzcar_blank[i,]$drug_exposure_id = temp[1,]$drug_exposure_id
  mmzcar_blank[i,]$person_id = temp[1,]$person_id
  mmzcar_blank[i,]$drug_concept_id = temp[1,]$drug_concept_id
  mmzcar_blank[i,]$drug_exposure_start_date = (temp$drug_exposure_start_date)[1]
  mmzcar_blank[i,]$drug_exposure_end_date = last(temp$drug_exposure_end_date)
  mmzcar_blank[i,]$total_quantity = sum(temp$quantity)
}

tail(mmzcar_blank)

dbRemoveTable(conn = con,
              name = "mmzcar2")

dbWriteTable(conn = con, 
             name = "mmzcar2", 
             value = mmzcar_blank)

"
SELECT * 
FROM mmzcar2
" -> sql

mmzcar2 <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype)))

"
select drug_exposure_id, mmzcar2.person_id, drug_concept_id, episode, drug_exposure_start_date, drug_exposure_end_date, total_quantity, 
gender_source_value,
(extract(year from drug_exposure_start_date) - year_of_birth + 1) AS age_at_drug_exposure_start_date 
from mmzcar2 
LEFT JOIN @schema.person person
ON mmzcar2.person_id = person.person_id
"-> sql

mmzcar3 <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname))

"
select * from mmzcar3 as episode
left join
(select distinct(episode.episode) as episode1, avg(value_as_number) as avgweight
from mmzcar3 as episode
inner join @schema.measurement
on episode.person_id = measurement.person_id
where measurement_concept_id in (@weight)
group by episode1
order by episode1) as avgweight
on episode.episode = avgweight.episode1
left join
(select distinct(episode.episode) as episode2, avg(value_as_number) as avgheight
from mmzcar3 as episode
inner join @schema.measurement
on episode.person_id = measurement.person_id
where measurement_concept_id in (@height)
group by episode2
order by episode2) as avgheight
on episode.episode = avgheight.episode2
"-> sql
mmzcar4 <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname, weight = weight, height = height))

dbRemoveTable(conn = con,
              name = "mmzcar4")

dbWriteTable(conn = con, 
             name = "mmzcar4", 
             value = mmzcar4)


"select episode, drug_exposure_id, person_id, drug_concept_id, drug_exposure_start_date, drug_exposure_end_date,
total_quantity, gender_source_value, age_at_drug_exposure_start_date, avgweight, avgheight, (avgweight/power(avgheight/100,2)) as bmi
from mmzcar4" -> sql
mmzcar5 <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype)))

dbRemoveTable(conn = con,
              name = "mmzcar5")

dbWriteTable(conn = con, 
             name = "mmzcar5", 
             value = mmzcar5)

# 췌장염 약물 공변량 class 1A

# atc_cd

# "select mmzcar5.episode, mmzcar5.drug_exposure_id, mmzcar5.person_id, mmzcar5.drug_concept_id,
# mmzcar5.drug_exposure_start_date, mmzcar5.drug_exposure_end_date, mmzcar5.total_quantity,
# mmzcar5.gender_source_value, mmzcar5.age_at_drug_exposure_start_date, mmzcar5.avgweight,
# mmzcar5.avgheight, mmzcar5.bmi, m.drug_concept_id as class1a_drug_concept_id, m.atc_cd
# from mmzcar5
# left join (select * from @schema.drug_exposure where atc_cd in ('C02AB', 'A07EC03', 'C10AB02', 'N03AX24', 'H03BB01', 'N02AA08', 'L01BC01', 'L01BC01', 'J04BA02', 'C09AA02',
#              'C03CA01', 'J04AC01', 'A07EC02', 'J01XD01', 'P01CX01', 'C10AA03', 'C01BA02', 'A11HA02', 'C10AA01',
#              'P01CB02', 'J01EC01', 'M01AB02', 'D06AA04', 'N03AG01')) m
# on mmzcar5.person_id = m.person_id
# WHERE m.drug_exposure_start_date BETWEEN (mmzcar5.drug_exposure_start_date) AND (mmzcar5.drug_exposure_end_date)
# ORDER BY episode, atc_cd
# " -> sql
# 
# mmzcar7 <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname))

#drug_concept_id

"select mmzcar5.episode, mmzcar5.drug_exposure_id, mmzcar5.person_id, mmzcar5.drug_concept_id,
mmzcar5.drug_exposure_start_date, mmzcar5.drug_exposure_end_date, mmzcar5.total_quantity,
mmzcar5.gender_source_value, mmzcar5.age_at_drug_exposure_start_date, mmzcar5.avgweight,
mmzcar5.avgheight, mmzcar5.bmi, m.drug_concept_id as class1a_drug_concept_id, m.atc_cd
from mmzcar5
left join (select * from @schema.drug_exposure where drug_concept_id in 
('968429','1707348','1236610','957136','40238238','1311104','1311078','41301497','19064967','1353578','19078859','968465','19061326','19022174',
'19080187','19022104','44084557','19022959','40175400','40165762','35603226','40237529','44032873','1782573','40165789','42481598',
'1711792','40175390','40238246','41051797','1539407','968467','40175394','41242653','36884836','1236611','40864885','40959912',
'44135259','43645162','40958344','1539411','43278193','41240556')) m

on mmzcar5.person_id = m.person_id
WHERE m.drug_exposure_start_date BETWEEN (mmzcar5.drug_exposure_start_date) AND (mmzcar5.drug_exposure_end_date)
ORDER BY episode, drug_concept_id
" -> sql

mmzcar7 <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname))



# 췌장염 약물 공변량 class 1B drug

#atc_cd

# "select mmzcar5.episode, mmzcar5.drug_exposure_id, mmzcar5.person_id, mmzcar5.drug_concept_id,
# mmzcar5.drug_exposure_start_date, mmzcar5.drug_exposure_end_date, mmzcar5.total_quantity,
# mmzcar5.gender_source_value, mmzcar5.age_at_drug_exposure_start_date, mmzcar5.avgweight,
# mmzcar5.avgheight, mmzcar5.bmi, m.drug_concept_id as class1b_drug_concept_id, m.atc_cd
# from mmzcar5
# left join (select * from @schema.drug_exposure where atc_cd in ('L01XX14', 'C01BD01', 'L04AX01', 'G03GB02', 'S01BA01', 'L01AA06', 'J05AF05', 
#             'C09CA01', 'G03AC02', 'G03CA01', 'L01BB02', 'H03BB02', 'J05AE04', 'G03AC01', 'A02BC01', 'G03CA57', 'J01EE01')) m
# on mmzcar5.person_id = m.person_id
# WHERE m.drug_exposure_start_date BETWEEN (mmzcar5.drug_exposure_start_date) AND (mmzcar5.drug_exposure_end_date)
# ORDER BY episode, atc_cd
# " -> sql

# mmzcar8 <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname))

"select mmzcar5.episode, mmzcar5.drug_exposure_id, mmzcar5.person_id, mmzcar5.drug_concept_id,
mmzcar5.drug_exposure_start_date, mmzcar5.drug_exposure_end_date, mmzcar5.total_quantity,
mmzcar5.gender_source_value, mmzcar5.age_at_drug_exposure_start_date, mmzcar5.avgweight,
mmzcar5.avgheight, mmzcar5.bmi, m.drug_concept_id as class1b_drug_concept_id, m.atc_cd
from mmzcar5
left join (select * from @schema.drug_exposure where drug_concept_id in ('1436651','903649','19029028','36895264','40220483','19101742',
'1719010','1836433','1309992','40185276','1704186','19101743','19018977',
'19077125','19014880','40185304','42628951','40236647')) m
on mmzcar5.person_id = m.person_id
WHERE m.drug_exposure_start_date BETWEEN (mmzcar5.drug_exposure_start_date) AND (mmzcar5.drug_exposure_end_date)
ORDER BY episode, drug_concept_id
" -> sql

mmzcar8 <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname))




# 췌장염 약물 공변량 class 2 drug

# "select mmzcar5.episode, mmzcar5.drug_exposure_id, mmzcar5.person_id, mmzcar5.drug_concept_id,
# mmzcar5.drug_exposure_start_date, mmzcar5.drug_exposure_end_date, mmzcar5.total_quantity,
# mmzcar5.gender_source_value, mmzcar5.age_at_drug_exposure_start_date, mmzcar5.avgweight,
# mmzcar5.avgheight, mmzcar5.bmi, m.drug_concept_id as class2_drug_concept_id, m.atc_cd
# from mmzcar5
# left join (select * from @schema.drug_exposure where atc_cd in ('N02BE01', 'C03AA04', 'N05AH02', 'J05AF02', 'D10AF02', 'G03C', 'L01XX02', 
#             'L01XX24', 'N01AX10', 'L02BA01')) m
# on mmzcar5.person_id = m.person_id
# WHERE m.drug_exposure_start_date BETWEEN (mmzcar5.drug_exposure_start_date) AND (mmzcar5.drug_exposure_end_date)
# ORDER BY episode, atc_cd
# 
# " -> sql
# 
# mmzcar9 <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname))

"select mmzcar5.episode, mmzcar5.drug_exposure_id, mmzcar5.person_id, mmzcar5.drug_concept_id,
mmzcar5.drug_exposure_start_date, mmzcar5.drug_exposure_end_date, mmzcar5.total_quantity,
mmzcar5.gender_source_value, mmzcar5.age_at_drug_exposure_start_date, mmzcar5.avgweight,
mmzcar5.avgheight, mmzcar5.bmi, m.drug_concept_id as class2_drug_concept_id, m.atc_cd
from mmzcar5
left join (select * from @schema.drug_exposure where drug_concept_id in ('43863992','800881','19072235','1724919',
'19072157','1127078','21090567','40220393','1436683','800892',
'800921','40220876','19072176','19021146','19084909','19075608',
'40220389','19019851','19096574','21021955')) m
on mmzcar5.person_id = m.person_id
WHERE m.drug_exposure_start_date BETWEEN (mmzcar5.drug_exposure_start_date) AND (mmzcar5.drug_exposure_end_date)
ORDER BY episode, drug_concept_id

" -> sql

mmzcar9 <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname))

mmzcar5$class1a = rep(0,nrow(mmzcar5))
mmzcar5$class1b = rep(0,nrow(mmzcar5))
mmzcar5$class2 = rep(0,nrow(mmzcar5))

# class 1a

for (i in 1:ep) {if (i %in% mmzcar7$episode) {
  temp = mmzcar7 %>% filter(mmzcar7$episode == i)
  mmzcar5[i,]$class1a = length(unique(temp$drug_concept_id))}
  else {mmzcar5[i,]$class1a = 0}}

# class 1b

for (i in 1:ep) {if (i %in% mmzcar8$episode) {
  temp = mmzcar8 %>% filter(mmzcar8$episode == i)
  mmzcar5[i,]$class1b = length(unique(temp$drug_concept_id))}
  else {mmzcar5[i,]$class1b = 0}}

# class 2

for (i in 1:ep) {if (i %in% mmzcar9$episode) {
  temp = mmzcar9 %>% filter(mmzcar9$episode == i)
  mmzcar5[i,]$class2 = length(unique(temp$drug_concept_id))}
  else {mmzcar5[i,]$class2 = 0}}


# Charlson Comorbidity Index (CCI code)
## weight 1

"select *
from mmzcar5
left join (select * from @schema.condition_occurrence WHERE ext_cond_source_value_kcd LIKE 'I21%'
	OR ext_cond_source_value_kcd LIKE 'I22%'
	OR ext_cond_source_value_kcd LIKE 'I43%'
	OR ext_cond_source_value_kcd LIKE 'I50%'
	OR ext_cond_source_value_kcd LIKE 'I70%'
	OR ext_cond_source_value_kcd LIKE 'I71%'
	OR ext_cond_source_value_kcd LIKE 'G45%'
	OR ext_cond_source_value_kcd LIKE 'G46%'
	OR ext_cond_source_value_kcd LIKE 'I6%'
	OR ext_cond_source_value_kcd LIKE 'F00%'
	OR ext_cond_source_value_kcd LIKE 'F03%'
	OR ext_cond_source_value_kcd LIKE 'G30%'
	OR ext_cond_source_value_kcd LIKE 'J40%'
	OR ext_cond_source_value_kcd LIKE 'J41%'
	OR ext_cond_source_value_kcd LIKE 'J42%'
	OR ext_cond_source_value_kcd LIKE 'J43%'
	OR ext_cond_source_value_kcd LIKE 'J44%'
	OR ext_cond_source_value_kcd LIKE 'J45%'
	OR ext_cond_source_value_kcd LIKE 'J46%'
	OR ext_cond_source_value_kcd LIKE 'J47%'
	OR ext_cond_source_value_kcd LIKE 'J60%'
	OR ext_cond_source_value_kcd LIKE 'J61%'
	OR ext_cond_source_value_kcd LIKE 'J62%'
	OR ext_cond_source_value_kcd LIKE 'J63%'
	OR ext_cond_source_value_kcd LIKE 'J64%'
	OR ext_cond_source_value_kcd LIKE 'J65%'
	OR ext_cond_source_value_kcd LIKE 'J66%'
	OR ext_cond_source_value_kcd LIKE 'J67%'
	OR ext_cond_source_value_kcd LIKE 'M05%'
	OR ext_cond_source_value_kcd LIKE 'M06%'
	OR ext_cond_source_value_kcd LIKE 'M32%'
	OR ext_cond_source_value_kcd LIKE 'M33%'
	OR ext_cond_source_value_kcd LIKE 'M34%'
	OR ext_cond_source_value_kcd LIKE 'K25%'
	OR ext_cond_source_value_kcd LIKE 'K26%'
	OR ext_cond_source_value_kcd LIKE 'K27%'
	OR ext_cond_source_value_kcd LIKE 'K28%'
	OR ext_cond_source_value_kcd LIKE 'B18%'
	OR ext_cond_source_value_kcd LIKE 'K73%'
	OR ext_cond_source_value_kcd LIKE 'K74%'
	OR ext_cond_source_value_kcd IN ('I252', 'I110', 'I130', 'I099', 'I132', 'I255', 'I420', 'I425', 'I426', 'I427', 'I428', 'I429', 'P290',
									 'I731', 'I738', 'I739', 'I771', 'I790', 'I792', 'K551', 'K558', 'K559', 'Z958', 'Z959', 'H340', 'F051', 
									 'G311', 'I278', 'I279', 'J684', 'J701', 'J703', 'M315', 'M351', 'M353', 'M360', 'K700', 'K701', 'K702', 
									 'K703', 'K709', 'K713', 'K714', 'K715', 'K717', 'K760', 'K762', 'K763', 'K764', 'K768', 'K769', 'Z944', 
									 'E100', 'E101', 'E106', 'E108', 'E109', 'E110', 'E111', 'E116', 'E118', 'E119', 'E120', 'E121', 'E121', 
									 'E126', 'E128', 'E129', 'E130', 'E131', 'E136', 'E138', 'E139', 'E140', 'E141', 'E146', 'E148', 'E149')
	AND condition_status_source_value = 'Y'
)co

on mmzcar5.person_id = co.person_id			--Join condition table
WHERE co.condition_start_date BETWEEN (mmzcar5.drug_exposure_start_date - INTERVAL '182 days') AND (mmzcar5.drug_exposure_start_date)
ORDER BY episode, co.condition_start_date

" -> sql
cci1 <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname))
cci1 <- cci1 %>% select(episode, condition_occurrence_id, condition_concept_id, condition_source_value, condition_source_concept_id, condition_status_source_value,
                        ext_cond_source_value_kcd)

## weight 2

"select *
from mmzcar5
left join (select * from @schema.condition_occurrence WHERE ext_cond_source_value_kcd LIKE 'G81%'
OR ext_cond_source_value_kcd LIKE 'G82%'
OR ext_cond_source_value_kcd LIKE 'N18%'
OR ext_cond_source_value_kcd LIKE 'N19%'
OR ext_cond_source_value_kcd LIKE 'C0%'
OR ext_cond_source_value_kcd LIKE 'C1%'
OR ext_cond_source_value_kcd LIKE 'C21%'
OR ext_cond_source_value_kcd LIKE 'C22%'
OR ext_cond_source_value_kcd LIKE 'C23%'
OR ext_cond_source_value_kcd LIKE 'C24%'
OR ext_cond_source_value_kcd LIKE 'C25%'
OR ext_cond_source_value_kcd LIKE 'C26%'
OR ext_cond_source_value_kcd LIKE 'C30%'
OR ext_cond_source_value_kcd LIKE 'C31%'
OR ext_cond_source_value_kcd LIKE 'C32%'
OR ext_cond_source_value_kcd LIKE 'C33%'
OR ext_cond_source_value_kcd LIKE 'C34%'
OR ext_cond_source_value_kcd LIKE 'C37%'
OR ext_cond_source_value_kcd LIKE 'C38%'
OR ext_cond_source_value_kcd LIKE 'C39%'
OR ext_cond_source_value_kcd LIKE 'C40%'
OR ext_cond_source_value_kcd LIKE 'C41%'
OR ext_cond_source_value_kcd LIKE 'C43%'
OR ext_cond_source_value_kcd LIKE 'C45%'
OR ext_cond_source_value_kcd LIKE 'C46%'
OR ext_cond_source_value_kcd LIKE 'C47%'
OR ext_cond_source_value_kcd LIKE 'C48%'
OR ext_cond_source_value_kcd LIKE 'C49%'
OR ext_cond_source_value_kcd LIKE 'C50%'
OR ext_cond_source_value_kcd LIKE 'C51%'
OR ext_cond_source_value_kcd LIKE 'C52%'
OR ext_cond_source_value_kcd LIKE 'C53%'
OR ext_cond_source_value_kcd LIKE 'C54%'
OR ext_cond_source_value_kcd LIKE 'C55%'
OR ext_cond_source_value_kcd LIKE 'C56%'
OR ext_cond_source_value_kcd LIKE 'C57%'
OR ext_cond_source_value_kcd LIKE 'C58%'
OR ext_cond_source_value_kcd LIKE 'C6%'
OR ext_cond_source_value_kcd LIKE 'C70%'
OR ext_cond_source_value_kcd LIKE 'C71%'
OR ext_cond_source_value_kcd LIKE 'C72%'
OR ext_cond_source_value_kcd LIKE 'C73%'
OR ext_cond_source_value_kcd LIKE 'C74%'
OR ext_cond_source_value_kcd LIKE 'C75%'
OR ext_cond_source_value_kcd LIKE 'C76%'
OR ext_cond_source_value_kcd LIKE 'C81%'
OR ext_cond_source_value_kcd LIKE 'C82%'
OR ext_cond_source_value_kcd LIKE 'C83%'
OR ext_cond_source_value_kcd LIKE 'C84%'
OR ext_cond_source_value_kcd LIKE 'C85%'
OR ext_cond_source_value_kcd LIKE 'C88%'
OR ext_cond_source_value_kcd LIKE 'C90%'
OR ext_cond_source_value_kcd LIKE 'C91%'
OR ext_cond_source_value_kcd LIKE 'C92%'
OR ext_cond_source_value_kcd LIKE 'C93%'
OR ext_cond_source_value_kcd LIKE 'C94%'
OR ext_cond_source_value_kcd LIKE 'C95%'
OR ext_cond_source_value_kcd LIKE 'C96%'
OR ext_cond_source_value_kcd LIKE 'C97%'
OR ext_cond_source_value_kcd IN ('E102', 'E103', 'E104', 'E105', 'E107', 'E112', 'E115', 'E117', 'E122', 'E123', 'E124', 'E125', 'E127', 'E132',
'E133', 'E134', 'E135', 'E137', 'E142', 'E143', 'E144', 'E145', 'E147', 'G041', 'G114', 'G801', 'G802', 'G830',
'G831', 'G832', 'G833', 'G834', 'G839', 'I120', 'I131', 'I120', 'N032', 'N033', 'N034', 'N035', 'N036', 'N037',
'N032', 'N052', 'N053', 'N054', 'N055', 'N056', 'N057', 'N250', 'Z490', 'Z491', 'Z492', 'Z940', 'Z992')
	AND condition_status_source_value = 'Y'
)co

on mmzcar5.person_id = co.person_id			--Join condition table
WHERE co.condition_start_date BETWEEN (mmzcar5.drug_exposure_start_date - INTERVAL '182 days') AND (mmzcar5.drug_exposure_start_date)
ORDER BY episode, co.condition_start_date

" -> sql
cci2 <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname))
cci2 <- cci2 %>% select(episode, condition_occurrence_id, condition_concept_id, condition_source_value, condition_source_concept_id, condition_status_source_value,
                        ext_cond_source_value_kcd)

## weight 3

"select *
from mmzcar5
left join (select * from @schema.condition_occurrence WHERE ext_cond_source_value_kcd IN 
('I850', 'I859', 'I864', 'I982', 'K704', 'K711', 'K721', 'K729', 'K765', 'K766', 'K767')
AND condition_status_source_value = 'Y'
)co
on mmzcar5.person_id = co.person_id			--Join condition table
WHERE co.condition_start_date BETWEEN (mmzcar5.drug_exposure_start_date - INTERVAL '182 days') AND (mmzcar5.drug_exposure_start_date)
ORDER BY episode, co.condition_start_date

" -> sql
cci3 <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname))
cci3 <- cci3 %>% select(episode, condition_occurrence_id, condition_concept_id, condition_source_value, condition_source_concept_id, condition_status_source_value,
                        ext_cond_source_value_kcd)


## weight 6

"select *
from mmzcar5
left join (select * from @schema.condition_occurrence WHERE ext_cond_source_value_kcd LIKE 'C77%'
OR ext_cond_source_value_kcd LIKE 'C78%'
OR ext_cond_source_value_kcd LIKE 'C79%'
OR ext_cond_source_value_kcd LIKE 'C80%'
OR ext_cond_source_value_kcd LIKE 'B20%'
OR ext_cond_source_value_kcd LIKE 'B21%'
OR ext_cond_source_value_kcd LIKE 'B22%'
OR ext_cond_source_value_kcd LIKE 'B24%'
AND condition_status_source_value = 'Y'
)co
on mmzcar5.person_id = co.person_id			--Join condition table
WHERE co.condition_start_date BETWEEN (mmzcar5.drug_exposure_start_date - INTERVAL '182 days') AND (mmzcar5.drug_exposure_start_date)
ORDER BY episode, co.condition_start_date

" -> sql
cci6 <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname))
cci6 <- cci6 %>% select(episode, condition_occurrence_id, condition_concept_id, condition_source_value, condition_source_concept_id, condition_status_source_value,
                        ext_cond_source_value_kcd)

target = mmzcar5 %>% select(episode) %>% distinct() %>% pull
cci_score_1 = data.frame(episode=numeric(), score=numeric())
for (i in target){
  s = cci1 %>% filter(episode==i) %>% select(condition_source_value) %>% distinct %>% count
  cci_score_1 = rbind(cci_score_1, data.frame(episode=i, score=s))
}
cci_score_1
cci_score_1 = cci_score_1 %>% rename(cci_score_1 = n)
mmzcar5 = left_join(mmzcar5, cci_score_1, by=('episode'))
mmzcar5

target = cci2 %>% select(episode) %>% distinct() %>% pull
cci_score_2 = data.frame(episode=numeric(), score=numeric())
for (i in target){
  s = cci2 %>% filter(episode==i) %>% select(condition_source_value) %>% distinct %>% count
  cci_score_2 = rbind(cci_score_2, data.frame(episode=i, score=s))
}
cci_score_2 = cci_score_2 %>% rename(cci_score_2 = n)
mmzcar5 = left_join(mmzcar5, cci_score_2, by=('episode'))

target = cci3 %>% select(episode) %>% distinct() %>% pull
cci_score_3 = data.frame(episode=numeric(), score=numeric())
for (i in target){
  s = cci3 %>% filter(episode==i) %>% select(condition_source_value) %>% distinct %>% count
  cci_score_3 = rbind(cci_score_3, data.frame(episode=i, score=s))
}
cci_score_3 = cci_score_3 %>% rename(cci_score_3 = n)
mmzcar5 = left_join(mmzcar5, cci_score_3, by=('episode'))

target = cci6 %>% select(episode) %>% distinct() %>% pull
cci_score_6 = data.frame(episode=numeric(), score=numeric())

for (i in target){
  s = cci6 %>% filter(episode==i) %>% select(condition_source_value) %>% distinct %>% count
  cci_score_6 = rbind(cci_score_6, data.frame(episode=i, score=s))
}
cci_score_6 = cci_score_6 %>% rename(cci_score_6 = n)
mmzcar5 = left_join(mmzcar5, cci_score_6, by=('episode'))

mmzcar5$cci_score_1 = ifelse(is.na(mmzcar5$cci_score_1),0,mmzcar5$cci_score_1)
mmzcar5$cci_score_2 = ifelse(is.na(mmzcar5$cci_score_2),0,mmzcar5$cci_score_2)
mmzcar5$cci_score_3 = ifelse(is.na(mmzcar5$cci_score_3),0,mmzcar5$cci_score_3)
mmzcar5$cci_score_6 = ifelse(is.na(mmzcar5$cci_score_6),0,mmzcar5$cci_score_6)

mmzcar5$cci_score = mmzcar5$cci_score_1+mmzcar5$cci_score_2+mmzcar5$cci_score_3+mmzcar5$cci_score_6
mmzcarfinal = mmzcar5 %>% select(-c(cci_score_1, cci_score_2, cci_score_3, cci_score_6))
hist(mmzcarfinal$cci_score)


# 공변량 no.4 Hypertriglyceridemia : TG 1000 이상, 1000 이하로 나누어서 볼 것

mmzcarfinal
"
select *
from mmzcarfinal
left join (
SELECT person_id as person_id2, measurement_id, measurement_concept_id,
measurement_date, value_as_number, measurement_source_value
FROM @schema.measurement
where measurement_concept_id = '3022192') m
on mmzcarfinal.person_id = m.person_id2			--Join measurement table
WHERE m.measurement_date BETWEEN (mmzcarfinal.drug_exposure_start_date) AND (mmzcarfinal.drug_exposure_end_date)
ORDER BY episode, m.measurement_date
" -> sql
tg <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname))
tg



nrow(tg)
tgabnormal <- tg %>% subset(value_as_number > 200)
tgnormal <- tg %>% subset(value_as_number <= 200)


nrow(tgabnormal)
nrow(tgnormal)

length(unique(mmzcarfinal$episode))
length(unique(tg$episode))
length(unique(tgabnormal$episode))
length(unique(tgnormal$episode))


length(unique(tg$episode))
hist(tg$value_as_number, breaks = c(0,200, 1138), freq = TRUE)
max(tg$value_as_number)

target = tg %>% select(episode) %>% distinct() %>% pull
tg_value = data.frame(episode=numeric(), tgmax = numeric())

for (i in target){
  maxtg = tg %>% filter(episode==i) %>% select(value_as_number) %>% max()
  tg_value = rbind(tg_value, data.frame(episode=i, tgmax=maxtg))
}

mmzcarfinal = left_join(mmzcarfinal, tg_value, by=('episode'))

dbRemoveTable(conn = con, 
              name = "mmzcarfinal")

dbWriteTable(conn = con, 
             name = "mmzcarfinal", 
             value = mmzcarfinal)

#### Exclusion Criteria


### 췌장염 EXCLUSION CRITERIA
# Index date 이전 6개월 동안 Acute pancreatitis에 영향을 줄 수 있는 질환을 진단받은 적이 있는 경우 제외
# Alcohol 관련 상병: 모두 Exclusion  
# 췌장암 상병: 모두 exclusion

# ICD10 code로 조회

mmzcarfinal$count
"
SELECT distinct(episode) FROM mmzcarfinal AS episode
INNER JOIN(
  SELECT *
    FROM @schema.condition_occurrence
  WHERE ext_cond_source_value_kcd in ('C250','C251','C252','C253','C254','C259','C788','D0170','F100','F1000','F101','F102',
                                      'F1020','F1024','F105','F106','F107','F108','F109','G312','G405','G621','G721','G991',
                                      'H570','I426','K292','K700','K701','K702','K703','K704','K709','K852','K860') 
)co
ON episode.person_id = co.person_id			--Join condition table

WHERE condition_start_date BETWEEN drug_exposure_start_date- INTERVAL '182 days' AND drug_exposure_start_date
" -> sql
pancalcex <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname))
nrow(pancalcex)

dbRemoveTable(conn = con,
              name = "pancalcex")

dbWriteTable(conn = con, 
             name = "pancalcex", 
             value = pancalcex)

"
select * from mmzcarfinal
where episode in (
select episode FROM mmzcarfinal
except
select episode from pancalcex)
" -> sql
excludepancalcex <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype)))
nrow(excludepancalcex)

excludepancalcex <- excludepancalcex %>% select(-c(row.names))

dbRemoveTable(conn = con,
              name = "excludepancalcex")

dbWriteTable(conn = con, 
             name = "excludepancalcex", 
             value = excludepancalcex)

## Gall stone 관련 상병 있으면서 간기능 이상 동반시 exclusion
# (다시 말해 index date 이전 6개월 동안 Gall stone 관련 상병 있으면서 AND
#  index date 이전 1개월동안 AST > UNL*2배(=80) OR ALT > UNL*2배(=80) 있는 경우 exclusion)

# ICD10 code로 조회

"
SELECT distinct(episode) FROM excludepancalcex AS episode
INNER JOIN(
  SELECT *
  FROM @schema.condition_occurrence
  WHERE ext_cond_source_value_kcd in ('K563', 'K800', 'K801', 'K802', 'K803', 'K804', 'K805', 'K851') 
)co
ON episode.person_id = co.person_id

INNER JOIN(
	SELECT *
	FROM @schema.measurement WHERE measurement_concept_id in (3006923, 3042781)) m

ON episode.person_id = m.person_id	

WHERE measurement_date BETWEEN (drug_exposure_start_date - INTERVAL '30 days') AND (drug_exposure_start_date)
and value_as_number > 80 and condition_start_date BETWEEN drug_exposure_start_date- INTERVAL '182 days' AND drug_exposure_start_date
"-> sql

gallstoneex <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname))
nrow(gallstoneex)

dbRemoveTable(conn = con,
              name = "gallstoneex")

dbWriteTable(conn = con, 
             name = "gallstoneex", 
             value = gallstoneex)

"
select * from excludepancalcex
where episode in (
select episode FROM excludepancalcex
except
select episode from gallstoneex)
" -> sql
excludepancalcgalex <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype)))
nrow(excludepancalcgalex)

excludepancalcgalex <- excludepancalcgalex %>% select(-c(row.names))

dbRemoveTable(conn = con,
              name = "excludepancalcgalex")

dbWriteTable(conn = con, 
             name = "excludepancalcgalex", 
             value = excludepancalcgalex)

# ② Index date 이전 1개월 동안 Serum Amylase > UNL(=100) OR Serum lipase > UNL(=60) 있으면 제외
"
select distinct(episode) from excludepancalcgalex
  INNER JOIN(
    SELECT *
      FROM @schema.measurement WHERE measurement_concept_id in (3016771)) m
  
  ON excludepancalcgalex.person_id = m.person_id	
  
  WHERE measurement_date BETWEEN (drug_exposure_start_date - INTERVAL '30 days') AND (drug_exposure_start_date)
  and value_as_number > 100

"->sql

amyex <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname))

"
select distinct(episode) from excludepancalcgalex
  INNER JOIN(
    SELECT *
      FROM @schema.measurement WHERE measurement_concept_id in (3004905)) m
  
  ON excludepancalcgalex.person_id = m.person_id	
  
  WHERE measurement_date BETWEEN (drug_exposure_start_date - INTERVAL '30 days') AND (drug_exposure_start_date)
  and value_as_number > 60
"->sql
lipex <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname))

amylipex <- union(amyex, lipex)

dbRemoveTable(conn = con,
              name = "amylipex")

dbWriteTable(conn = con, 
             name = "amylipex", 
             value = amylipex)

amylipex <- amylipex %>% select(episode)

"
select * from excludepancalcgalex
  where episode not in (select episode from amylipex)
"->sql
pancalcgalamylipex <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype)))

pancalcgalamylipex <- pancalcgalamylipex %>% select(-c(row.names))

nrow(pancalcgalamylipex)

pancalcgalamylipex %>% avgweight()

pancalcgalamylipex$bmi[is.na(pancalcgalamylipex$bmi)] <- "N/A"
pancalcgalamylipex$avgweight[is.na(pancalcgalamylipex$avgweight)] <- "N/A"
pancalcgalamylipex$avgheight[is.na(pancalcgalamylipex$avgheight)] <- "N/A"
pancalcgalamylipex$tgmax[is.na(pancalcgalamylipex$tgmax)] <- "N/A"

dbRemoveTable(conn = con,
              name = "pancalcgalamylipex")

dbWriteTable(conn = con, 
             name = "pancalcgalamylipex", 
             value = pancalcgalamylipex)

######################################
# Cohort

# <<사건 발생의 정의>>
#   
# index date 부터 ( drug_end_date + washout period ) 동안  ① 또는  ② 에 해당하는 경우
# mild cohort
# ① Serum amylase > UNL(=100) OR Serum lipase > UNL(=60)
# ② Acute pancreatitis 관련 진단명 최초 부여된 경우

# extreme cohort
# ① Serum amylase > UNL(=300) OR Serum lipase > UNL(=180)
# ② Acute pancreatitis 관련 진단명 최초 부여된 경우


# mild cohort

"
(select distinct(episode) from pancalcgalamylipex
 inner join (select * from @schema.measurement where measurement_concept_id in (3016771)) m
 on pancalcgalamylipex.person_id = m.person_id
 WHERE measurement_date BETWEEN  (drug_exposure_start_date + INTERVAL '2 days') AND (drug_exposure_end_date + INTERVAL '56 days')
 and value_as_number > 100)
union

(select distinct(episode) from pancalcgalamylipex
  inner join (select * from @schema.measurement where measurement_concept_id in (3004905)) m
  on pancalcgalamylipex.person_id = m.person_id
  WHERE measurement_date BETWEEN  (drug_exposure_start_date + INTERVAL '2 days') AND (drug_exposure_end_date + INTERVAL '56 days')
  and value_as_number > 60)

union 

(select distinct(episode) from pancalcgalamylipex
  inner join (select * from @schema.condition_occurrence where condition_concept_id in (199074, 4340961, 4340960)) m
  on pancalcgalamylipex.person_id = m.person_id
  WHERE condition_start_date BETWEEN (drug_exposure_start_date + INTERVAL '2 days') AND (drug_exposure_end_date + INTERVAL '56 days')
)
"-> sql
mildepisode <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname))
mildepisode

# severe cohort

"
(select distinct(episode) from pancalcgalamylipex
 inner join (select * from @schema.measurement where measurement_concept_id in (3016771)) m
 on pancalcgalamylipex.person_id = m.person_id
 WHERE measurement_date BETWEEN  (drug_exposure_start_date + INTERVAL '2 days') AND (drug_exposure_end_date + INTERVAL '56 days')
 and value_as_number > 300)
union

(select distinct(episode) from pancalcgalamylipex
  inner join (select * from @schema.measurement where measurement_concept_id in (3004905)) m
  on pancalcgalamylipex.person_id = m.person_id
  WHERE measurement_date BETWEEN  (drug_exposure_start_date + INTERVAL '2 days') AND (drug_exposure_end_date + INTERVAL '56 days')
  and value_as_number > 180)

union 

(select distinct(episode) from pancalcgalamylipex
  inner join (select * from @schema.condition_occurrence where condition_concept_id in (199074, 4340961, 4340960)) m
  on pancalcgalamylipex.person_id = m.person_id
  WHERE condition_start_date BETWEEN (drug_exposure_start_date + INTERVAL '2 days') AND (drug_exposure_end_date + INTERVAL '56 days')
)
"-> sql
severeepisode <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname))
severeepisode

# mild measurement_id

"
(select distinct(measurement_id) from pancalcgalamylipex
 inner join (select * from @schema.measurement where measurement_concept_id in (3016771)) m
 on pancalcgalamylipex.person_id = m.person_id
 WHERE measurement_date BETWEEN  (drug_exposure_start_date + INTERVAL '2 days') AND (drug_exposure_end_date + INTERVAL '56 days')
 and value_as_number > 100)
union

(select distinct(measurement_id) from pancalcgalamylipex
  inner join (select * from @schema.measurement where measurement_concept_id in (3004905)) m
  on pancalcgalamylipex.person_id = m.person_id
  WHERE measurement_date BETWEEN  (drug_exposure_start_date + INTERVAL '2 days') AND (drug_exposure_end_date + INTERVAL '56 days')
  and value_as_number > 60)
"-> sql
mildmeasurementid <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname))
mildmeasurementid

# severe measurement_id

"
(select distinct(measurement_id) from pancalcgalamylipex
 inner join (select * from @schema.measurement where measurement_concept_id in (3016771)) m
 on pancalcgalamylipex.person_id = m.person_id
 WHERE measurement_date BETWEEN  (drug_exposure_start_date + INTERVAL '2 days') AND (drug_exposure_end_date + INTERVAL '56 days')
 and value_as_number > 300)
union

(select distinct(measurement_id) from pancalcgalamylipex
  inner join (select * from @schema.measurement where measurement_concept_id in (3004905)) m
  on pancalcgalamylipex.person_id = m.person_id
  WHERE measurement_date BETWEEN  (drug_exposure_start_date + INTERVAL '2 days') AND (drug_exposure_end_date + INTERVAL '56 days')
  and value_as_number > 180)
"-> sql

severemeasurementid <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname))
severemeasurementid

# total measurement table

"
(select * from pancalcgalamylipex
inner join (select * from @schema.measurement where measurement_concept_id in (3016771, 3004905)) m
on pancalcgalamylipex.person_id = m.person_id
WHERE measurement_date BETWEEN  (drug_exposure_start_date + INTERVAL '2 days') AND (drug_exposure_end_date + INTERVAL '56 days')
order by episode, measurement_date)
"-> sql

measurementtotal <- dbGetQuery(con, render(translate(sql, targetDialect = mydbtype), schema = myschemaname))
measurementtotal <- measurementtotal %>% select(c(episode, drug_exposure_id, 4, drug_concept_id, drug_exposure_start_date, drug_exposure_end_date,
                              total_quantity, gender_source_value, age_at_drug_exposure_start_date, avgweight, avgheight,
                              bmi, class1a, class1b, class2, cci_score, tgmax, measurement_id,
                              measurement_concept_id, measurement_date, value_as_number, measurement_source_value))

# mild measurement table

measurement_mild <- measurementtotal %>% subset(measurement_id %in% mildmeasurementid$measurement_id) %>% arrange(episode, measurement_date)

# extreme measurement table

measurement_severe <- measurementtotal %>% subset(measurement_id %in% severemeasurementid$measurement_id) %>% arrange(episode, measurement_date)

#######################################
data <- pancalcgalamylipex


#### ANALYSIS ###################

### DATA PREP FOR SURVIVAL ANALYSIS

data$bmi <- as.numeric(data$bmi)
data$tgmax <- as.numeric(data$tgmax)
data$avgweight <- as.numeric(data$avgweight)
data$avgheight <- as.numeric(data$avgheight)


data <- data %>% mutate(age_group = ifelse (age_at_drug_exposure_start_date >= 90, "90 -", 
                                            ifelse(age_at_drug_exposure_start_date >= 80, "80 - 89",
                                                   ifelse(age_at_drug_exposure_start_date >= 70, "70 - 79",
                                                          ifelse(age_at_drug_exposure_start_date >= 60, "60 - 69",
                                                                 ifelse(age_at_drug_exposure_start_date >= 50, "50 - 59",
                                                                        ifelse(age_at_drug_exposure_start_date >= 40, "40 - 49",
                                                                               ifelse(age_at_drug_exposure_start_date >= 30, "30 - 39",
                                                                                      ifelse(age_at_drug_exposure_start_date >= 20, "20 - 29",
                                                                                             ifelse(age_at_drug_exposure_start_date >= 10, "10 - 19",
                                                                                                    "0 - 9"))))))))))

data <- data %>% rename(sex = gender_source_value)

data <- data %>% mutate(age_group_2 = ifelse (age_at_drug_exposure_start_date >= 40, "over 40", "under 40"))

data$age_group_2 <- factor(data$age_group_2)

data <- data %>% mutate(age_group_3 = ifelse (age_at_drug_exposure_start_date >= 60, "60 - ",
                                              ifelse(age_at_drug_exposure_start_date >= 40, "40 - 59",
                                                     ifelse(age_at_drug_exposure_start_date >= 20, "20 - 39",
                                                            "0 - 19"))))
data$age_group_3 <- factor(data$age_group_3)

data <- data %>% mutate(tgmax_group = ifelse(is.na(tgmax) == TRUE, NA, ifelse (tgmax >= 1000, "over 1000",
                                                              ifelse(tgmax >= 200, "200 - 1000", "under 200"))))
data$tgmax_group <- factor(data$tgmax_group)

data <- data %>% mutate(bmi_group = ifelse (is.na(bmi) == TRUE ,NA, ifelse (bmi >= 30, "Obese",
                                              ifelse(bmi >= 25, "Overweight", 
                                                     ifelse(bmi >= 18.5, "Normal", "Underweight")))))


data$bmi_group <- factor(data$bmi_group)


data <- data %>% mutate(mildevent = ifelse (episode %in% mildepisode$episode, 1, 0))
data <- data %>% mutate(severeevent = ifelse (episode %in% severeepisode$episode, 1, 0))

mild_event_date <- as.Date(0, origin = "1900-01-01")
severe_event_date <- as.Date(0, origin = "1900-01-01")

data <- cbind(data, mild_event_date, severe_event_date)
data$episode <- as.integer(data$episode)

str(data)

for (i in mildepisode$episode) {
    temp <- measurement_mild %>% subset(measurement_mild$episode == i)
    data[match(i,data$episode),]$mild_event_date = as.Date(temp[1,]$measurement_date)
  }


for (i in severeepisode$episode) {
    temp <- measurement_severe %>% subset(measurement_severe$episode == i)
    data[match(i,data$episode),]$severe_event_date = as.Date(temp[1,]$measurement_date)
  }

data$drug_exposure_start_date <- as.Date(data$drug_exposure_start_date)
data$drug_exposure_end_date <- as.Date(data$drug_exposure_end_date)
data$mild_event_date <- as.Date(data$mild_event_date)
data$severe_event_date <- as.Date(data$severe_event_date)

data['days_before_mild_event'] <- NA
data['days_before_severe_event'] <- NA


for (i in data$episode) {
  temp <- data %>% subset(data$episode == i)
  d = ifelse (temp$mildevent == 1, 
              (temp$mild_event_date - temp$drug_exposure_start_date), 
              (temp$drug_exposure_end_date - temp$drug_exposure_start_date))
  data[match(i,data$episode),]$days_before_mild_event = d
}

for (i in data$episode) {
  temp <- data %>% subset(data$episode == i)
  d = ifelse (temp$severeevent == 1, 
              (temp$severe_event_date - temp$drug_exposure_start_date), 
              (temp$drug_exposure_end_date - temp$drug_exposure_start_date))
  data[match(i,data$episode),]$days_before_severe_event = d
}

##################################################################################

write.csv(data, "data.csv", row.names = FALSE)
write.csv(measurement_mild, "measurement_mild.csv", row.names = FALSE)
write.csv(measurement_severe, "measurement_severe.csv", row.names = FALSE)
write.csv(mildepisode, "mildepisode.csv", row.names = FALSE)
write.csv(severeepisode, "severeepisode.csv", row.names = FALSE)
write.csv(measurementtotal, "measurementtotal.csv", row.names = FALSE)
write.csv(mildmeasurementid, "mildmeasurementid.csv", row.names = FALSE)
write.csv(severemeasurementid, "severemeasurementid.csv", row.names = FALSE)

##############################################################################

