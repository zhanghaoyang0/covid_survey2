ssh compute9
cd /home/yanglab_data/user/zhanghy/project/github_zhanghy/covid_survey2
conda activate r421
R

#====================================================================
# prepare 
#====================================================================
source('./code/prepare.r')

#====================================================================
# clean weather
#====================================================================
df = read.xlsx('./data/raw/wuxi_weather.xlsx', sheet=1)
df = df%>%mutate(date = as.Date(paste(year, month, day, sep='-'), format="%Y-%m-%d"))%>%select(-station, -year, -month, -day)
weather = df

#====================================================================
# clean medical staff 
#====================================================================
# load
df1 = read.xlsx('data/raw/staff.xlsx', sheet=1)
df2 = read.xlsx('data/raw/staff.xlsx', sheet=2)

# # see dup
# df1[df1$id1%in%get_dup(df1$id1),]
# df2[df2$name%in%get_dup(df2$name),]

# clean df1
df1 = df1%>%mutate(group=ifelse(group%in%c('检验', '药剂'), '技师', group))%>%
    mutate(group=ifelse(!group%in%c('医生', '技师', '护理'), '其他', group))

# clean df2
df2 = df2%>%mutate(department=ifelse(grepl('护士|输液', department), '护理', department))%>%
    mutate(department=ifelse(grepl('处|属|办|外包|供应室', department), '其他', department))%>%
    mutate(department=ifelse(grepl('康复|影像|实验|检验|药', department), '技师', department))%>%
    mutate(department=ifelse(grepl('陈晓音|侯蓉|秦凯芹|王丹霞|王佳丽|吴雯娟', name), '护理', department))%>%
    mutate(department=ifelse(!grepl('护理|其他|技师', department), '医生', department))%>%
    mutate(code=ifelse(is.na(code), '绿码', code))

# merge, reset id
df1 = df1%>%select(id1, group, start, end)%>%mutate(month=12)%>%mutate(code=NA)%>%rename(id=id1)
df2 = df2%>%select(name, department, start, end, code)%>%mutate(month=11)%>%rename(id=name, group=department)
df = rbind(df2, df1)
set.seed(123)
temp = sample(1:nrow(df), nrow(df)) 
df$id = temp # id is person-time but not person

# fix date 
df$start = as.Date(sapply(df$start, fix_date), origin = "1970-01-01")
df$end = as.Date(sapply(df$end, fix_date), origin = "1970-01-01")
df[is.na(df$end), 'end'] =  df[is.na(df$end), 'start'] + 7

# chn to eng 
table(df$group)
df = df%>%select(id, group, start, end)
temp = df$group
temp[temp=='医生'] = 'Doctor'; temp[temp=='护理'] = 'Nurse'; temp[temp=='技师'] = 'Technician'; temp[temp=='其他'] = 'Other'
df$group = temp

staff = df

#====================================================================
# clean outpat data
# OP_EM_MARK: 1、门诊，2、复诊，3、急诊
#====================================================================

# read data, clean
df = df_raw = clean_df(read.csv('data/raw/门诊（2022.11-12）.csv', fileEncoding='utf-8'))
dict = clean_df(read.xlsx('data/raw/dict.xlsx', sheet=1))
dict1 = read.xlsx('data/raw/dict.xlsx', sheet=3)

# date
df = df%>%mutate(SEE_DOC_DT=as.Date(SEE_DOC_DT), BIRTHDAY=as.Date(BIRTHDAY))%>%
    mutate(age=(SEE_DOC_DT-BIRTHDAY)/365, after_1207=ifelse(SEE_DOC_DT>"2022-12-7", 1, 0))
# translate
df$DPT_NAME = gsub(' ', '', df$DPT_NAME)
for (i in 1:nrow(dict1)){
    col = dict1[i, 'col']; key = dict1[i, 'key']; val = dict1[i, 'value']
    df[df[,col]==key, col] = val
}
table(df$DPT_NAME)

# PRES_ILLN
df$epi = clean_PRES_ILLN(df$PRES_ILLN)

# drop cols
drop_cols = c('MED_REC_ID', 'CARD_TYPE', 'CARD_NO', 'OCCUP_CODE', 'GUARD_NAME', 'RELAT_CODE', 'OP_EM_NO', 'INT_MARK',
 'MAIN_SYMP_T', 'MAIN_SYMP_U', 'SECO_SYMP_T', 'SECO_SYMP_U', 'DPT_CODE', 'SBP', 'DBP', 'BIRTHDAY', 'PRES_ILLN', 'DISE_DESC', 'DIS_CODE_1', 'DIS_NAME_1', 
 'AUXI_EX', 'OTHER_HIST', 'MD_DIS_CODE', 'MOBILE', 'HEART_RATE', 'BREATGE', 'PULSE')
df[,drop_cols] = NULL

for (i in names(df)){
    if (!i %in% dict$字段名称){next}
    temp = dict%>%filter(字段名称==i)%>%select(字段名称, 数据项, 定义)
    print(temp[c(1,2,3)])
}
outpat = df


#====================================================================
# clean inpat data
# OP_EM_MARK: 1、门诊，2、复诊，3、急诊
#====================================================================
# read data, clean
df = df_raw = clean_df(read.csv('data/raw/住院（2022.11-12）.csv', fileEncoding='utf-8'))
dict = clean_df(read.xlsx('data/raw/dict.xlsx', sheet=2))
dict1 = read.xlsx('data/raw/dict.xlsx', sheet=4)

# date
df = df%>%mutate(IN_HP_DT=as.Date(IN_HP_DT), OUT_HP_DT=as.Date(OUT_HP_DT), BIRTHDAY=as.Date(BIRTHDAY))%>%
    mutate(age=(IN_HP_DT-BIRTHDAY)/365, after_1207=ifelse(IN_HP_DT>"2022-12-7", 1, 0))

# translate
df[is.na(df$DPT_NAME), 'DPT_NAME'] = '儿科'
for (i in 1:nrow(dict1)){
    col = dict1[i, 'col']; key = dict1[i, 'key']; val = dict1[i, 'value']
    df[df[,col]==key, col] = val
}
table(df$DPT_NAME)

# drop cols
drop_cols = c(get('CARD', names(df)), get('ADDR', names(df)), 'DPT_CODE')
df[,drop_cols] = NULL

for (i in names(df)){
    if (!i %in% dict$字段名称){next}
    temp = dict%>%filter(字段名称==i)%>%select(字段名称, 数据项, 定义)
    print(temp[c(1,2,3)])
}


inpat = df
#====================================================================
# save
#====================================================================
save(staff, weather, outpat, inpat, file='data/data.rdata')

#====================================================================
# load
#====================================================================
datas = load('data/data.rdata')
datas