ssh compute9
cd /home/yanglab_data/user/zhanghy/project/github_zhanghy/covid_survey2
conda activate r421
R

#====================================================================
# prepare 
#====================================================================
source('./code/prepare.r')
path_raw = '/home/yanglab_data/user/zhanghy/project/github_zhanghy/db/wuxi/covid_survey2_rawData/'

#====================================================================
# clean weather
#====================================================================
df = read.xlsx(paste0(path_raw, 'wuxi_weather.xlsx'), sheet=1)
df = df%>%mutate(DT = as.Date(paste(year, month, day, sep='-'), format="%Y-%m-%d"))%>%select(-station, -year, -month, -day)
weather = df

#====================================================================
# clean medical staff 
#====================================================================
# load
df1 = read.xlsx(paste0(path_raw, 'staff.xlsx'), sheet=1)
df2 = read.xlsx(paste0(path_raw, 'staff.xlsx'), sheet=2)

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
df = df_raw = clean_df(read.csv(paste0(path_raw, '门诊（2022.11-12）.csv'), fileEncoding='utf-8'))
dict = clean_df(read.xlsx(paste0(path_raw, 'dict.xlsx'), sheet=1))
map = read.xlsx(paste0(path_raw, 'dict.xlsx'), sheet=3)
map1 = read.xlsx(paste0(path_raw, 'dict.xlsx'), sheet=4)%>%filter(group=='outpat')
map2 = read.xlsx(paste0(path_raw, 'dict.xlsx'), sheet=5)

# date
df = df%>%mutate(SEE_DOC_DT=as.Date(SEE_DOC_DT), BIRTHDAY=as.Date(BIRTHDAY))%>%
    mutate(age=round(as.numeric((SEE_DOC_DT-BIRTHDAY)/365), 2))

# chn to eng, dpt
col = 'DPT_NAME'
vec = df[, col]; vec = gsub(' ', '', vec); vec = chn_to_eng(vec, map)
vec[vec%in%c('Respiratory', 'Infectious')] = 'Respiratory / Infectious' 
table(vec)
df[, col] = vec

# set id for name
col = 'DOC_NAME'
vec = df[, col]; vec = chn_to_eng(vec, map2, F)
table(vec)
df[, col] = vec
df = df%>%rename(PAT_NAME=NAME)%>%mutate(PAT_NAME=1:nrow(df))

# PRES_ILLN
df$epi = clean_PRES_ILLN(df$PRES_ILLN)
df[df$SEE_DOC_DT<'2022-12-10', 'epi'] = 'nega' # one covid before 1210, may be typo

# drop cols
drop_cols = c('MED_REC_ID', 'CARD_TYPE', 'CARD_NO', 'OCCUP_CODE', 'GUARD_NAME', 'RELAT_CODE', 'OP_EM_NO', 'INT_MARK',
 'MAIN_SYMP_T', 'MAIN_SYMP_U', 'SECO_SYMP_T', 'SECO_SYMP_U', 'DPT_CODE', 'SBP', 'DBP', 'BIRTHDAY', 'PRES_ILLN', 'DISE_DESC', 'DIS_CODE_1', 'DIS_NAME_1', 
 'AUXI_EX', 'OTHER_HIST', 'MD_DIS_CODE', 'MOBILE', 'HEART_RATE', 'BREATGE', 'PULSE', 'MAIN_SYMP', 'PRES_DRUGS', 'OP_EM_MARK')
df[,drop_cols] = NULL

for (i in names(df)){
    if (!i %in% dict$字段名称){next}
    temp = dict%>%filter(字段名称==i)%>%select(字段名称, 数据项, 允许值或说明)
    print(temp[c(1,2,3)])
}

# rename
df = df%>%rename(DT=SEE_DOC_DT, DIS=MD_DIS_NAME)

# recode
df$SEX = ifelse(df$SEX==1, 'm', 'f')

outpat = df

#====================================================================
# clean inpat data
#====================================================================
# read data, clean
df = df_raw = clean_df(read.csv(paste0(path_raw, '住院（2022.11-12）.csv'), fileEncoding='utf-8'))
dict = clean_df(read.xlsx(paste0(path_raw, 'dict.xlsx'), sheet=2))
map = read.xlsx(paste0(path_raw, 'dict.xlsx'), sheet=3)
map1 = read.xlsx(paste0(path_raw, 'dict.xlsx'), sheet=4)%>%filter(group=='inpat')
map2 = read.xlsx(paste0(path_raw, 'dict.xlsx'), sheet=5)

# date
df = df%>%mutate(IN_HP_DT=as.Date(IN_HP_DT), OUT_HP_DT=as.Date(OUT_HP_DT), BIRTHDAY=as.Date(BIRTHDAY))%>%
    mutate(age=round(as.numeric((IN_HP_DT-BIRTHDAY)/365), 2), hosp_day=as.numeric(OUT_HP_DT-IN_HP_DT))

# chn to eng, dpt
for (col in c('OUT_DPT_NAME', 'IN_DPT_NAME')){
    vec = df[, col]; vec = gsub(' ', '', vec); vec = chn_to_eng(vec, map)
    vec[vec%in%c('Respiratory', 'Infectious', 'Internal')] = 'Respiratory / Infectious' # Internal is 阳性病房，属于Infectious or Respiratory
    print(table(vec)); print(sum(is.na(vec)))
    df[, col] = vec
}

# set id for name
for (col in c('HPHY_NAME', 'PRIMARY_NUR', 'OP_DOC_NAME')){
    vec = df[, col]; vec = chn_to_eng(vec, map2, F)
    print(table(vec))
    df[, col] = vec
}

# keep cols and rows
drop_cols = c(get('CARD|ADDR|TRABSFERRED', names(df)), get('PHONE|DIS_CODE|QUCT|OPER|WOUND|NNIS', names(df), exact=T),
 'DPT_CODE', 'DPT_NAME', 'SCHEAD_NAME', 'REC_NAME', 'RCD_DT', 'POF_DT', 'JOB_CODE', 'CON_NAME', 'OUT_HP_DT')
df[,drop_cols] = NULL

keep_col = c('HP_S_NO', 'age', 'SEX', 'PAT_DIA_NAME', 'IN_DPT_NAME', 'IN_HP_DT', 'HPHY_NAME', 'PRIMARY_NUR', 'TOTAL_COST', 
    'hosp_day', 'OP_DOC_NAME', get('SF|FEE', names(df), exact=T))
df = df[, keep_col]

# fee
df = df%>%filter(TOTAL_COST!=0)
df$DMM_FEE = df$CH_USE_DMM_FEE+df$TR_USE_DMM_FEE+df$OP_USE_DMM_FEE
df$GEN_MED_FEE = df$GEN_MED_SF+df$GEN_CUR_SF
df[,c('NON_OP_TCP_FEE','ANES_FEE', 'OP_FEE','ANT_COST_FEE', 'CH_USE_DMM_FEE', 'TR_USE_DMM_FEE', 'OP_USE_DMM_FEE', 'GEN_MED_SF', 'GEN_CUR_SF')]=NULL

for (i in names(df)){
    if (!i %in% dict$字段名称){next}
    temp = dict%>%filter(字段名称==i)%>%select(字段名称, 数据项, 允许值或说明)
    print(temp[c(1,2,3)])
}

# rename
df = df%>%rename(DPT_NAME=IN_DPT_NAME, DT=IN_HP_DT, DIS=PAT_DIA_NAME, OTH_TRE_FEE=OTH_TRE_SF)

# recode
df$SEX = ifelse(df$SEX==1, 'm', 'f')


inpat = df

#====================================================================
# temp
#====================================================================
t1 = outpat
t2 = inpat

#====================================================================
# second clean
#====================================================================
# clean disease
clean_dis = function(DIS){
    p1 = "\\[.*?\\]"; p2 = "\\(.*?\\)"
    DIS = gsub('\\\\', '', DIS)
    DIS = gsub('多发性|急性|慢性|浅表性|化脓性|发热性|糜烂性|间歇性|', '', DIS)
    DIS[grepl('化学治疗|健康|咨询|准备', DIS)] = 'Other'
    DIS[grepl('过敏|荨麻|湿疹|特应|变应', DIS)] = 'Allergic'
    DIS[grepl('肿瘤|软骨瘤', DIS)] = 'Neoplastic'
    DIS[grepl('牙|齿|扁桃体', DIS)] = 'Oral'
    DIS[grepl('屈光|眼|视', DIS)] = 'Ophthalmology'
    DIS[grepl('新生儿|婴儿|早产|高危', DIS)&!grepl('非新生儿', DIS)] = 'Neonatal'
    DIS[grepl('神|惊|癫', DIS)&!grepl('非新生儿', DIS)] = 'Neuropsychiatric'
    DIS[grepl('支气管炎', DIS)] = 'Bronchitis'
    DIS[grepl('肺炎', DIS)] = 'Pneumonia'
    DIS[grepl('呼吸道感染', DIS)] = 'URTI'
    DIS = sapply(DIS, function(x){x1 = gsub(p1, "", gsub(p2, "", x)); return(x1)}) # remove string in ()|[], keep item before sep
    DIS = sapply(DIS, function(x){x1 = strsplit(x, ',|，|不伴')[[1]][1]; return(x1)}) # keep item before sep
    DIS = sapply(DIS, function(x){x1 = strsplit(x, '伴')[[1]][1]; return(x1)}) # keep item before sep
    return(DIS)
}

show_top = function(vec, n=10, exclude = c('URTI', 'Bronchitis', 'Pneumonia', 'Other')){
    DIS = clean_dis(vec); tab = table(DIS)
    top = sort(tab/length(DIS), decreasing=T)[1:n]
    temp = names(top)
    temp = temp[!temp%in%exclude]
    return(temp)
}

inpat$DIS = clean_dis(inpat$DIS)
outpat$DIS = clean_dis(outpat$DIS)

inpat1 = filter_period(inpat)
outpat1 = filter_period(outpat)
show_top(inpat1$DIS)
show_top(outpat1$DIS)


keep = c('URTI', 'Bronchitis', 'Pneumonia', 'Neonatal', 'Neuropsychiatric', 'Allergic', 'Oral', 'Ophthalmology')
inpat$DIS[!inpat$DIS%in%keep] = 'Other'
outpat$DIS = ifelse(outpat$DIS%in%keep, outpat$DIS, 'Other')
#====================================================================
# save
#====================================================================
dim(staff); dim(weather); dim(inpat); dim(outpat)
head(staff); head(weather); head(inpat); head(outpat)

save(staff, outpat, inpat, weather, file='data/data.rdata')

#====================================================================
# load
#====================================================================
datas = load('data/data.rdata')
datas
