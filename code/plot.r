ssh compute9
cd /home/yanglab_data/user/zhanghy/project/github_zhanghy/covid_survey2
conda activate r421
R

#====================================================================
# prepare 
#====================================================================
source('./code/prepare.r')

#====================================================================
# load
#====================================================================
datas = load('data/data.rdata')
datas

#====================================================================
# plot: outpat time series
#====================================================================
sort(table(outpat$DPT_NAME), decreasing=T)
sort(table(inpat$DPT_NAME), decreasing=T)

groups1 = c('All', 'Other', 'Emergency', 'Internal', 'Respiratory', 'Infectious', 'COVID positive', 'COVID contact history')
groups2 = c('All', 'Other', 'Internal', 'Respiratory', 'Infectious')
nvisit_outpat = get_nvisit_bygroup(outpat, 'SEE_DOC_DT', 'DPT_NAME', days, groups1)
nvisit_inpat = get_nvisit_bygroup(inpat, 'IN_HP_DT', 'DPT_NAME', days, groups2)

# outpat nvisit
plots = plot_nvist(nvisit_outpat, list(c('All', 'Other', 'Emergency', 'Internal', 'Respiratory', 'Infectious')))
p = ggarrange(plots[[1]], ncol=1, nrow=1) 
png('./plot/outpat_nvisit.png',height=400, width=800, res=120)
print(p)
dev.off()
# outpat covid
plots = plot_nvist(nvisit_outpat, list(c('COVID positive', 'COVID contact history')))
p = ggarrange(plots[[1]], ncol=1, nrow=1) 
png('./plot/outpat_covid.png',height=400, width=800, res=120)
print(p)
dev.off()
# inpat nvisit
plots = plot_nvist(nvisit_inpat, list(c('All', 'Other', 'Internal', 'Respiratory', 'Infectious')))
p = ggarrange(plots[[1]], ncol=1, nrow=1) 
png('./plot/inpat_nvisit.png',height=400, width=800, res=120)
print(p)
dev.off()

# need to check !!!
inpat%>%filter(DPT_NAME=='Internal')




sub = staff%>%filter(start<=day&end>=day)
for (group in c('Doctor', 'Nurse', 'Technician', 'Other')){
    n = sum(sub$group==group)
    out = c(out, day, group, n)
}



#====================================================================
# clean inpat data
#====================================================================
df = clean_df(read.csv('data/住院（2022.11-12）.csv', fileEncoding='utf-8'))
df = df%>%filter(!is.na(DPT_NAME))
dict = clean_df(read.xlsx('data/dict.xlsx', sheet=2))
dict1 = read.xlsx('data/dict.xlsx', sheet=4)

# date
df = df%>%mutate(IN_HP_DT=as.Date(IN_HP_DT), BIRTHDAY=as.Date(BIRTHDAY))%>%
    mutate(age=(IN_HP_DT-BIRTHDAY)/365, after_1207=ifelse(IN_HP_DT>"2022-12-7", 1, 0))
# translate
df$DPT_NAME = gsub(' ', '', df$DPT_NAME)
for (i in 1:nrow(dict1)){
    col = dict1[i, 'col']; key = dict1[i, 'key']; val = dict1[i, 'value']
    df[df[,col]==key, col] = val
}
sort(table(df$DPT_NAME))



# drop cols
drop_cols = names(df)[grepl('ADDR|HR|CARD|PHONE|WOUND|AGE', names(df))]
for (i in drop_cols){
    if (!i %in% dict$字段名称){next}
    temp = dict%>%filter(字段名称==i)%>%select(字段名称, 数据项, 定义)
    print(temp[,2])
}
df[,drop_cols] = NULL
for (i in names(df)){
    if (!i %in% dict$字段名称){next}
    temp = dict%>%filter(字段名称==i)%>%select(字段名称, 数据项, 定义)
    print(temp[,c(1,2)])
}



#====================================================================
# plot: time series
#====================================================================
# plot: number of patient
out = c()
dpts = c('All', 'Respiratory', 'Cardiology', 'Neonatology', 'Gastroenterology')
for (day in days){
    sub = df%>%filter(IN_HP_DT==day)
    for (dpt in dpts){
        if (dpt=='All'){num = nrow(sub)}
        else if (dpt=='All expect emergency medicine'){num = nrow(sub%>%filter(DPT_NAME!='Emergency medicine'))}
        else {num = nrow(sub%>%filter(DPT_NAME==dpt))}
        out = c(out, day, dpt, num)
    }
}

df_time = data.frame(matrix(out, ncol=3, byrow=T))%>%rename(day=X1, dpt=X2, num=X3)%>%
    mutate_if(is_numeric,as.numeric)%>%mutate(day=as.Date(day,origin="1970-01-01"))

plots = list() 
groups = list(c('All', 'Respiratory', 'Cardiology', 'Neonatology', 'Gastroenterology'))

for (i in 1:length(groups)){
    group = groups[[i]]
    df_p = df_time%>%filter(dpt%in%groups[[i]])
    ymax = ceiling(max(df_p$num)/100)*120
    df_text = df_p%>%filter(day==as.Date('2022-12-07'))%>%filter(num==max(num))%>%unique()%>%mutate(num=ymax*0.9)
    p = ggplot(df_p, aes(x=day, y=num, group=dpt)) +
        geom_point(aes(color=dpt)) + geom_line(aes(color=dpt)) + 
        geom_vline(xintercept=as.Date('2022-12-07'), linetype='dashed', color='blue', size=1.5) +
        labs(x=("Date"), y=("Number of patients")) + ylim(0, ymax) +
        scale_x_date(date_breaks = "weeks" , date_labels = "%m-%d") +
        geom_text(data=df_text, label="Adjustment of zero-COVID policy*", vjust=0) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color="black"), 
            legend.title = element_text(size=1))+
        scale_fill_nejm()
    plots[[i]] = p
}

p1 <- ggarrange(plots[[1]],  ncol=1, nrow=1, legend="bottom")

png('./plot/line_inpat.png',height=800, width=800, res=120)
print(p1)
dev.off()