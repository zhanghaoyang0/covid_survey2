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
# analysis
#====================================================================
library(pracma)
library(xts)

out = c()
groups = c('All', 'All expect emergency', 'Emergency', 'Internal medicine', 'E.N.T.', 'Dermatology', 'Respiratory', 
    'Infectious disease', 'COVID-19 positive', 'COVID-19 contact history')
for (day in days){
    sub = outpat%>%filter(SEE_DOC_DT==day)
    for (group in groups){
        if (group=='All'){num = nrow(sub)}
        else if (group=='All expect emergency'){num = nrow(sub%>%filter(DPT_NAME!='Emergency'))}
        else if (group=='COVID-19 positive'){num = nrow(sub%>%filter(epi=='posi'))}
        else if (group=='COVID-19 contact history'){num = nrow(sub%>%filter(epi=='contact_posi'))}
        else {num = nrow(sub%>%filter(DPT_NAME==group))}
        out = c(out, day, group, num)
    }
}

df_time = data.frame(matrix(out, ncol=3, byrow=T))%>%rename(day=X1, group=X2, num=X3)%>%
    mutate_if(is_numeric,as.numeric)%>%mutate(day=as.Date(day,origin="1970-01-01"))

ts = df_time%>%filter(group=='All')%>%pull(num)
peaks = findpeaks(ts)
