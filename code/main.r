cd /home/yanglab_data/user/zhanghy/project/github_zhanghy/covid_survey2
conda activate r421
R

#====================================================================
# prepare 
#====================================================================
source('./code/prepare.r')
datas = load('data/data.rdata')
datas
options(width=150)

#====================================================================
# filter
#====================================================================
filter_period = function(df, nweek=2){
    out = df%>%filter(DT>=(adjust_day-nweek*7)&DT<(adjust_day+nweek*7))%>%
            mutate(policy = ifelse(DT >= adjust_day, 'After', 'Before'))%>%
            mutate(policy = factor(policy, levels=c('Before', 'After')))
    return(out)
}

outpat1 = filter_period(outpat)
inpat1 = filter_period(inpat)

#====================================================================
# vist: characteristics
#====================================================================
des_popChara = function(df){
    out = c()
    for (nweek in c(-2:1, 9)){ # 9 mean full range
        if (nweek==9){start = adjust_day-2*7; end = adjust_day+(1+1)*7} else 
            {start = adjust_day+nweek*7; end = adjust_day+(nweek+1)*7}
        sub = df%>%filter(DT>=start&DT<end)
        n = nrow(sub)
        range = paste0(start, ' to ', end-1)
        age = sprintf('%.2f ± %.2f', mean(sub$age), sd(sub$age))
        n_male = table(sub$SEX)[2]
        n = sprintf('%.0f (%.2f%%)', n, 100*n_male/n)
        out = c(out, range, n, age)
    }
    out = data.frame(matrix(out, ncol=3, byrow=T))
    names(out) = c('range', 'n(male%)', 'age')
    return(out)
}

compare_sex = function(df){
    chi = chisq.test(df$SEX, df$policy)
    print(sprintf('chisquare test for sex: chi = %.2f, p = %.2f', chi$statistic, chi$p.value))
}

compare_age = function(df, test_range){
    start = test_range[1]; end = test_range[2]
    x1 = df%>%filter(DT<adjust_day)%>%select(age)
    x2 = df%>%filter(DT>=start&DT<end)%>%select(age)
    t = t.test(x1, x2)
    print(sprintf('t test for age: t = %.2f, p = %.2f', t$statistic, t$p.value))
}


des_popChara(outpat)
des_popChara(inpat)

# temp = des_popChara(outpat)
# temp = des_popChara(inpat)
# write.csv(temp, './temp.csv', quote=F, row.names=F)

compare_sex(outpat1); compare_sex(inpat1)
compare_age(outpat1, test_range = c(as.Date('2022-12-15'), as.Date('2022-12-29')))
compare_age(inpat1, test_range = c(as.Date('2022-12-15'), as.Date('2022-12-29')))

#====================================================================
# nvist: data
# n doc	326
# n nurse 334
# n technician 142
# n other 300
#====================================================================
get_nvisit_bygroup = function(df, date_col, group_col, dates, groups){
    out = c()
    for (day in dates){
        sub = df[df[,date_col] == day, group_col]
        for (group in groups){
            if (group=='All'){num = length(sub)}
            else if (group=='All COVID'){num = sum(sub%in%c('posi', 'contact_posi')); group='All'}
            else if (group=='Other'){num = sum(!sub%in%groups)}
            else if (group=='COVID-19 positive'){num = sum(sub=='posi')}
            else if (group=='COVID-19 contact history'){num = sum(sub=='contact_posi')}
            else {num = sum(sub==group)}
            out = c(out, day, group, num)
        }
    }
    nvisit = data.frame(matrix(out, ncol=3, byrow=T))%>%rename(DT=X1, group=X2, num=X3)%>%
        mutate_if(is_numeric,as.numeric)%>%mutate(DT=as.Date(DT,origin="1970-01-01"))
    return(nvisit)
}

sort(table(outpat$DPT_NAME), decreasing=T)
sort(table(inpat$DPT_NAME), decreasing=T)

# nvist of patient
groups1 = c('All', 'Other', 'Emergency', 'Respiratory / Infectious')
groups2 = c('All COVID', 'COVID-19 positive', 'COVID-19 contact history')
groups3 = c('All', 'Other', 'Respiratory / Infectious')
nvisit_outpat = get_nvisit_bygroup(outpat, 'DT', 'DPT_NAME', days, groups1)
nposi_outpat = get_nvisit_bygroup(outpat, 'DT', 'epi', days, groups2)
nvisit_inpat = get_nvisit_bygroup(inpat, 'DT', 'DPT_NAME', days, groups3)

# num of med staff unable to work bc covid
out = c()
for (day in days){
    sub = staff%>%filter(start<=day&end>=day)
    for (group in c('All', 'Doctor', 'Nurse', 'Technician', 'Other')){
        if (group=='All'){n=nrow(sub)}else{n = sum(sub$group==group)}
        out = c(out, day, group, n)
    }
}
ncovid_staff = data.frame(matrix(out, ncol=3, byrow=T))%>%rename(DT=X1, group=X2, num=X3)%>%
    mutate_if(is_numeric,as.numeric)%>%mutate(DT=as.Date(DT,origin="1970-01-01"))

nvisit_outpat1 = reshape(nvisit_outpat, idvar = "DT", timevar = "group", direction = "wide")
nposi_outpat1 = reshape(nposi_outpat, idvar = "DT", timevar = "group", direction = "wide")
nvisit_inpat1 = reshape(nvisit_inpat, idvar = "DT", timevar = "group", direction = "wide")
ncovid_staff1 = reshape(ncovid_staff, idvar = "DT", timevar = "group", direction = "wide")
temp = nvisit_outpat1%>%merge(nvisit_inpat1, 'DT', all.x=T)%>%merge(nposi_outpat1, 'DT', all.x=T)%>%merge(ncovid_staff1, 'DT', all.x=T)
# write.csv(temp, './temp.csv', quote=F, row.names=F)

t1 = ncovid_staff1%>%filter(DT>=as.Date('2022-12-15')&DT<as.Date('2022-12-29'))%>%pull(num.All)
t2 = nposi_outpat1%>%filter(DT>=as.Date('2022-12-15')&DT<as.Date('2022-12-29'))%>%pull(num.All)

cor.test(t1, t2)

#====================================================================
# nvist: plot
#====================================================================
plot_nvist = function(df, groups, ylab_text, legend_pos, legend_col=1, re_level = F, y_inflat=1, x_text='2022-12-07'){
    df_p = df%>%filter(group%in%groups)
    if (re_level == T){df_p$group = factor(df_p$group, levels = groups)} # level group as groups
    ymax = ceiling(max(df_p$num)/100)*y_inflat*100
    df_text = df_p%>%filter(DT==as.Date(x_text))%>%filter(num==max(num))%>%mutate(num=ymax*0.9)
    df_text = df_text[1,]
    days1 = seq(as.Date("2022-11-03"), as.Date("2022-12-29"), by = "1 week")
    p = ggplot(df_p, aes(x=DT, y=num, group=group)) +
        geom_point(aes(color=group)) + geom_line(aes(color=group)) + 
        geom_vline(xintercept=as.Date('2022-12-15'), linetype='dashed', color='gray', size=1) +      
        ylim(0, ymax) + 
        scale_x_date(breaks = days1, date_labels = "%m-%d") +
        geom_text(data=df_text, label=" Policy \n adjustment", vjust=0.5, hjust=0.3, size=3.5) +
        ylab(ylab_text) + xlab('') +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color="black"), 
            axis.title.y = element_text(size = 10), panel.grid.major=element_blank(), 
            panel.grid.minor=element_blank(), # remove grid
            legend.title=element_blank(), legend.position = c(legend_pos[1], legend_pos[2])) +
        guides(color = guide_legend(ncol = legend_col)) + # legend row
        scale_color_manual(values = pal_npg('nrc')(5)) 
    return(p)
}

ylab1 = 'Number of outpatients'
ylab2 = 'Number of inpatients'
ylab3 = 'Number of patients with COVID-19 \n or COVID-19 contact history'
ylab4 = 'Number of Healthcare Providers'

p1 = plot_nvist(nvisit_outpat, c('All', 'Other', 'Respiratory / Infectious', 'Emergency'), 
    ylab_text=ylab1, legend_pos=c(0.25, 0.73), re_level=T, y_inflat=2.4)
p2 = plot_nvist(nvisit_inpat, c('All', 'Other', 'Respiratory / Infectious'), 
    ylab_text=ylab2, legend_pos=c(0.25, 0.78), y_inflat=1.65)
p3 = plot_nvist(nposi_outpat, c('All', 'COVID-19 positive', 'COVID-19 contact history'), 
    ylab_text=ylab3, legend_pos=c(0.25, 0.77), y_inflat=1.2)
p4 = plot_nvist(ncovid_staff, c('All', 'Doctor', 'Nurse', 'Technician', 'Other'), 
    ylab_text=ylab4, legend_pos=c(0.17, 0.67), re_level=T, y_inflat=1.5)

p = ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend=F, align = "hv", hjust=0.1, vjust=0.1) +
    theme(plot.margin = unit(c(0,0,0,0), "cm"))

# png('./plot/ts_nvist.png',height=900, width=1500, res=150)
# print(p)
# dev.off()

#====================================================================
# outpat inpat: dis prop
#====================================================================
## data
get_prop = function(df, keep_dis){
    out = c()
    for (i in unique(df$policy)){
        sub = df%>%filter(policy==i&DIS%in%keep_dis) 
        for (j in keep_dis){
            n = sum(sub$DIS==j)
            prop = n/nrow(sub)
            out = c(out, i, j, n, prop)
        }
    }
    res = data.frame(matrix(out, ncol=4, byrow=T))%>%mutate_if(is_numeric,as.numeric)%>%
        rename(policy=X1, group=X2, n=X3, prop=X4) 
    return(res)
}


plot_prop = function(df, title, lab_title, nrow_legend=2){
    p = ggplot(df, aes(x = policy, weight = prop, fill = group))+
        geom_bar(position = "stack") +
        xlab('') + ylab('') + 
        theme(plot.title = element_text(size = 10, hjust = 0.5),
            axis.text.y = element_text(color="black"), 
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 7.5)) +
        ggtitle(title) +
        coord_flip() +
        guides(fill=guide_legend(title=lab_title, nrow = nrow_legend)) + # legend row
        scale_fill_nejm()
    return(p)
}

keep1 = c('Pneumonia', 'URTI', 'Bronchitis', 'Other respiratory')
keep2 = c('ENT', 'Dermatology', 'Healthcare', 'Ophthalmology', 'General surgery', 'Other')
keep3 = c('Neurology', 'Neonatology', 'Nephropathy', 'Hematology', 'General surgery', 'Other')

outpat2 = get_prop(outpat1, keep1); outpat2[outpat2=='Other respiratory'] = 'Other'
inpat2 = get_prop(inpat1, keep1); inpat2[inpat2=='Other respiratory'] = 'Other'
outpat3 = get_prop(outpat1, keep2)
inpat3 = get_prop(inpat1, keep3)

temp = rbind(outpat2, inpat2, outpat3, inpat3)
temp = temp%>%mutate(prop1=sprintf('%.0f (%.2f%%)', n, prop*100))
# write.csv(temp, './temp.csv', quote=F, row.names=F)



# group with levels1
plots  = list()
plots[[1]] = plot_prop(outpat2, 'Respiratory disease(outpatient)', 'Disease', nrow_legend=4)
plots[[2]] = plot_prop(inpat2, 'Respiratory disease(inpatient)', 'Disease', nrow_legend=4)
plots[[3]] = plot_prop(outpat3, 'Non-respiratory disease(outpatient)', 'Disease', nrow_legend=6)
plots[[4]] = plot_prop(inpat3, 'Non-respiratory disease(inpatient)', 'Disease', nrow_legend=6)

p = ggarrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], hjust=0.1, vjust=0.1, ncol=2, nrow=2, common.legend=F, legend="right")

png('./plot/dis_prop1.png',height=700, width=1400, res=150)
p
dev.off()



# chisquare
sub1 = outpat1%>%filter(DIS%in%keep1)
sub2 = inpat1%>%filter(DIS%in%keep1)
sub3 = outpat1%>%filter(DIS%in%keep2)
sub4 = inpat1%>%filter(DIS%in%keep3)
with(sub1, chisq.test(DIS, policy))
with(sub2, chisq.test(DIS, policy))
with(sub3, chisq.test(DIS, policy))
with(sub4, chisq.test(DIS, policy))




#====================================================================
# inpat fee: prop
#====================================================================
## prop of cost
# recode fee col
inpat_fee2 = inpat1%>%mutate(DPT_NAME=ifelse(DPT_NAME=='Respiratory / Infectious', 'Respiratory / Infectious', 'Other'))
fee_cols = get('FEE', names(inpat1))
fee_cols1 = c() # fee > 5%
for (i in unique(inpat_fee2$policy)){
    for (j in unique(inpat_fee2$DPT_NAME)){
        sub = inpat_fee2%>%filter(policy==i&DPT_NAME==j)
        for (k in fee_cols){
            temp = sub[,k]
            prop = sum(temp)/sum(sub[, fee_cols])
            if (prop>0.05){fee_cols1 = c(fee_cols1, k)}
        }  
    }
}
fee_cols1 = unique(c(fee_cols1, 'OTHER_FEE')) # > 5% in either group
fee_cols2 = fee_cols[!fee_cols%in%fee_cols1] # merge to other

# fee prop
out = c()
for (i in unique(inpat_fee2$policy)){
    for (j in unique(inpat_fee2$DPT_NAME)){
        sub = inpat_fee2%>%filter(policy==i&DPT_NAME==j)      
        sub$OTHER_FEE = rowSums(sub[, c(fee_cols2, 'OTHER_FEE')])
        for (k in c(fee_cols1)){
            temp = sub[,k]
            mean = sprintf('%.2f±%.2f', mean(temp), sd(temp))
            prop = sum(temp)/sum(sub[,fee_cols1])
            out = c(out, i, j, k, mean, prop)
        }
    }
}
fee_prop = data.frame(matrix(out, ncol=5, byrow=T))%>%mutate_if(is_numeric,as.numeric)%>%
    rename(policy=X1, dpt=X2, group=X3, mean=X4, prop=X5)
key = c('NURSING_FEE', 'LAB_DIAG_FEE', 'IMAG_DIAG_FEE', 'OP_T_FEE', 'W_MED_FEE', 'DMM_FEE', 'GEN_MED_FEE', 'OTHER_FEE')
value = c('Nursing', 'Laboratory', 'Imaging', 'Surgery', 'Drug', 'Material', 'General', 'Other')
map = data.frame(group=key, group_new=value)
fee_prop = fee_prop%>%merge(map, 'group')%>%mutate(group=group_new)%>%select(-group_new)%>%
    mutate(group=factor(group, levels=c('Laboratory', 'Imaging', 'Nursing', 'Drug', 'Surgery', 'Material', 'General', 'Other'))) # rename fee

# write.csv(fee_prop, './temp.csv', quote=F, row.names=F)

# ## plot2: prop of cost, merge prop < 5% to other
# plots  = list()
# for (i in unique(fee_prop$dpt)){
#     title = ifelse(i=='Other', 'Respiratory / Infectious Diseases', 
#         'Other Diseases')
#     df_p1 = fee_prop%>%filter(dpt==i)
#     p = plot_prop(df_p1, title, 'Hospitalization expense', nrow_legend=8)
#     plots[[i]] = p 
# }
# p = ggarrange(plots[[1]], plots[[2]], hjust=0.1, vjust=0.1, ncol=1, nrow=2, common.legend=T, legend="right")



# png('./plot/fee_prop.png',height=500, width=1000, res=150)
# print(p)
# dev.off()


#====================================================================
# inpat cost and hosp duration
#====================================================================
## measure effect of policy on cost by regression
dpts = c('Total', "Gastroenterology", "Neonatology", "Neurology", "Nephropathy", "Cardiology", "Respiratory / Infectious") # dpt with patients > 100
inpat2 = rbind(inpat1, inpat1%>%mutate(DPT_NAME='Total')) # double df to add total
inpat2 = inpat2%>%mutate(DPT_NAME = ifelse(DPT_NAME%in%dpts, DPT_NAME, 'Other'))

desReg = function(df, col){
    df$y = df[,col]
    var1 = df%>%filter(policy=='Before')%>%pull(y)
    var2 = df%>%filter(policy=='After')%>%pull(y)
    mean1 = sprintf('%.2f ± %.2f', mean(var1), sd(var1))
    mean2 = sprintf('%.2f ± %.2f', mean(var2), sd(var2))
    lm = lm(y~policy+age+SEX, df)
    coef = summary(lm)$coefficients[2,]
    beta = sprintf('%.2f ± %.2f', coef[1], coef[2])
    p = sprintf('%.2f', coef[4])
    out = c(col, mean1, mean2, beta, p)
    return(out)
}

out = c()
for (dpt in c(dpts, 'Other')){
    for (col in c('TOTAL_COST', 'hosp_day')){
        sub = inpat2%>%filter(DPT_NAME==dpt)%>%select(policy, age, SEX, TOTAL_COST, hosp_day)
        reg = desReg(sub, col)
        out = c(out, dpt, reg)
    }
}

reg = data.frame(matrix(out, ncol=6, byrow=T)); names(reg) = c('dpt', 'col', 'mean_before', 'mean_after', 'beta', 'p')
# write.csv(reg, './temp.csv', quote=F, row.names=F)

#====================================================================
# comapre work load
#====================================================================
cal_workload = function(df, col){
    tab = table(df[,col], df$DT)
    out = data.frame(cbind(rbind(cbind('Before', tab[,1]), cbind('After', tab[,2]))))
    out = out%>%rename(policy=X1, n=X2)%>%mutate(n=as.numeric(n)/14)%>%filter(n>0) # divide length of period
    row.names(out) = NULL
    return(out)
}


cal_workload = function(df, col, name){
    out = c()
    for (day in unique(df$DT)){
        df1 = df%>%filter(DT==day)
        n = mean(table(df1[,col]))
        out = c(out, n)
    }
    res = data.frame(DT=unique(df$DT), n=out)



    tab = table(df[,col], df$policy)
    out = data.frame(cbind(name, rbind(cbind('Before', tab[,1]), cbind('After', tab[,2]))))
    out = out%>%rename(policy=V2, n=V3)%>%mutate(n=as.numeric(n)/14)%>%filter(n>0) # divide length of period
    row.names(out) = NULL
    return(out)
}

compare_workload = function(df, col){
    out = cal_workload(df, col)
    n1 = out%>%filter(policy=='Before')%>%pull(n)
    n2 = out%>%filter(policy=='After')%>%pull(n)
    test = t.test(n1, n2)
    mean1 = sprintf('%.2f ± %.2f', mean(n1), sd(n1))
    mean2 = sprintf('%.2f ± %.2f', mean(n2), sd(n2))
    stat = c(col, mean1, mean2, test$statistic, test$p.value)
    names(stat) = c('var', 'mean_before', 'mean_after', 't', 'p')
    return(stat)
}

dpts = c('all', 'other', 'Respiratory / Infectious')
compare = data.frame()
for (dpt in dpts){
    if (dpt=='Respiratory / Infectious'){outpat2 = outpat1%>%filter(DPT_NAME==dpt); inpat2 = inpat1%>%filter(DPT_NAME==dpt)}
    if (dpt=='all'){outpat2 = outpat1; inpat2 = inpat1}
    if (dpt=='other'){outpat2 = outpat1%>%filter(DPT_NAME!='Respiratory / Infectious'); inpat2 = inpat1%>%filter(DPT_NAME!='Respiratory / Infectious')}
    sub1 = compare_workload(outpat2, 'DOC_NAME')
    sub2 = compare_workload(inpat2, 'HPHY_NAME')
    # sub3 = compare_workload(inpat2, 'OP_DOC_NAME') # drop this in paper, number are too small
    sub4 = compare_workload(inpat2, 'PRIMARY_NUR')
    sub = data.frame(rbind(sub1, sub2, sub4))
    sub = cbind(dpt, sub)
    compare = rbind(compare, sub)   
}
# write.csv(compare, './temp.csv', quote=F, row.names=F)

#====================================================================
# plot work load by day
#====================================================================
df_p = data.frame()
for (dpt in dpts){
    if (dpt=='Respiratory / Infectious'){outpat2 = outpat1%>%filter(DPT_NAME==dpt); inpat2 = inpat1%>%filter(DPT_NAME==dpt)}
    if (dpt=='all'){outpat2 = outpat1; inpat2 = inpat1}
    if (dpt=='other'){outpat2 = outpat1%>%filter(DPT_NAME!='Respiratory / Infectious'); inpat2 = inpat1%>%filter(DPT_NAME!='Respiratory / Infectious')}
    x1 = colMeans(table(outpat2[, 'DOC_NAME'], outpat2$DT))
    x2 = colMeans(table(inpat2[, 'HPHY_NAME'], inpat2$DT))
    x3 = colMeans(table(inpat2[, 'PRIMARY_NUR'], inpat2$DT))
    dpt1 = str_to_title(gsub(' ', '', dpt))
    sub1 = data.frame(DT=names(x1), group=paste0('Doctor (', dpt1, ')'), num=x1, type='outpatient')
    sub2 = data.frame(DT=names(x2), group=paste0('Doctor (', dpt1, ')'), num=x2, type='inpatient')
    sub3 = data.frame(DT=names(x3), group=paste0('Doctor (', dpt1, ')'), num=x3, type='inpatient')
    df_p = rbind(df_p, sub1, sub2, sub3)
}
df_p = df_p%>%mutate(DT=as.Date(DT))
df_p1 = df_p%>%filter(type=='outpatient')
df_p2 = df_p%>%filter(type=='inpatient')
ylab1 = 'ttt'
ylab2 = 'ttt'


p2 = plot_nvist(df_p1, unique(df_p1$group), ylab_text=ylab1, legend_pos=c(0.2, 0.8), re_level=F, x_text='2022-12-08', y_inflat=0.05)

png('./plot/test.png',height=500, width=1000, res=150)
print(p)
dev.off()


ylab2 = 'Number of outpatients \n (Emergency)'
ylab3 = 'Number of outpatients \n (Respiratory / Infectious)'
ylab4 = 'Number of inpatients \n (All)'
ylab5 = 'Number of inpatients \n (Respiratory / Infectious)'



# p4 = plot_nvist(ncovid_staff, c('All', 'Doctor', 'Nurse', 'Technician', 'Other'), ylab_text=ylab4, legend_pos=c(0.17, 0.67), re_level=F, y_inflat=1.5)


#====================================================================
# nvist: analysis
#====================================================================
compare_nvist = function(df, prefixCol = 'NA'){ # nday is intervel is at day; prefix is first col
    weather1 = weather%>%select(DT, temp_ave, humi_ave)
    df = df%>%merge(weather1, 'DT')
    out = c()
    for (i in unique(df$group)){
        sub = df%>%filter(group==i)
        mod = lm(num ~ policy + temp_ave + humi_ave, data=sub)
        coef = summary(mod)$coefficients
        coef = coef[2, c(1,2,4)]
        coef1 = sprintf('%.2f ± %.2f', coef[1], coef[2]); coef2 = sprintf('%.2f', coef[3])
        temp1 = sub%>%filter(policy=='Before')%>%pull(num); temp2 = sub%>%filter(policy=='After')%>%pull(num)
        mean1 = sprintf('%.2f ± %.2f', mean(temp1), sd(temp1)); mean2 = sprintf('%.2f ± %.2f', mean(temp2), sd(temp2))
        out = c(out, prefixCol, i, mean1, mean2, coef1, coef2)
    }
    res = data.frame(matrix(out, ncol=6, byrow=T))
    names(res) = c('prefix', 'group', 'mean_before', 'mean_after', 'beta', 'p')
    return(res)
}

nvisit_inpat1 = filter_period(nvisit_inpat)
nvisit_outpat1 = filter_period(nvisit_outpat)

temp1 = compare_nvist(nvisit_inpat1, 'inpat')
temp2 = compare_nvist(nvisit_outpat1, 'outpat')
temp = rbind(temp1, temp2)

# write.csv(temp, './temp.csv', quote=F, row.names=F)

#====================================================================
# predict nvist
#====================================================================
lm_predict_nvist = function(df, group){
    df1 = filter_period(df)%>%filter(group==UQ(group))%>%
        mutate(holiday=ifelse(weekdays(DT)%in%c('Saturday', 'Sunday'), 1, 0))%>%
        merge(weather%>%select(temp_ave, humi_ave, DT), by='DT')
    df2 = df1%>%filter(policy=='Before'); df3 = df1%>%filter(policy=='After')
    reg = lm(num~temp_ave+humi_ave+holiday, df2)
    df3 = df3%>%mutate(num = predict(reg, newdata=df3))
    sub1 = df1%>%select(DT, num)%>%mutate(group='Actual')
    sub2 = df3%>%select(DT, num)%>%mutate(group='Predicted')
    df_p = rbind(sub1, sub2)
    return(df_p)
}

ylab1 = 'Number of outpatients \n (All)'
ylab2 = 'Number of outpatients \n (Emergency)'
ylab3 = 'Number of outpatients \n (Respiratory / Infectious)'
ylab4 = 'Number of inpatients \n (All)'
ylab5 = 'Number of inpatients \n (Respiratory / Infectious)'


nvisit_outpat1 = lm_predict_nvist(nvisit_outpat, 'All')
nvisit_outpat2 = lm_predict_nvist(nvisit_outpat, 'Emergency')
nvisit_outpat3 = lm_predict_nvist(nvisit_outpat, 'Respiratory / Infectious')
nvisit_outpat4 = lm_predict_nvist(nvisit_inpat, 'All')
nvisit_outpat5 = lm_predict_nvist(nvisit_inpat, 'Respiratory / Infectious')

p1 = plot_nvist(nvisit_outpat1, c('Actual', 'Predicted'), ylab_text = ylab1, legend_pos=c(0.15, 0.80), y_inflat=2.4, x_text='2022-12-14')
p2 = plot_nvist(nvisit_outpat2, c('Actual', 'Predicted'), ylab_text = ylab2, legend_pos=c(0.15, 0.80), y_inflat=2.4, x_text='2022-12-14')
p3 = plot_nvist(nvisit_outpat3, c('Actual', 'Predicted'), ylab_text = ylab3, legend_pos=c(0.15, 0.80), y_inflat=2.4, x_text='2022-12-14')
p4 = plot_nvist(nvisit_outpat4, c('Actual', 'Predicted'), ylab_text = ylab4, legend_pos=c(0.15, 0.80), y_inflat=2.4, x_text='2022-12-14')
p5 = plot_nvist(nvisit_outpat5, c('Actual', 'Predicted'), ylab_text = ylab5, legend_pos=c(0.15, 0.80), y_inflat=2.4, x_text='2022-12-14')


p = ggarrange(p1, p4, p3, p5, p2, ncol=2, nrow=3, common.legend=F, align = "hv", hjust=0.1, vjust=0.1) +
    theme(plot.margin = unit(c(0,0,0,0), "cm"))

png('./plot/pred_nvist.png',height=1200, width=1300, res=150)
print(p)
dev.off()


#====================================================================
# predict fee
#====================================================================
lm_predict_cost = function(df, group){
    if (group=='All clinical department'){df1 = df}else{df1 = df%>%filter(DPT_NAME==UQ(group))}
    df2 = df1%>%filter(policy=='Before'); df3 = df1%>%filter(policy=='After')
    reg = lm(TOTAL_COST~age+SEX+hosp_day, df2)
    df3 = df3%>%mutate(TOTAL_COST = predict(reg, newdata=df3))
    sub1 = df1%>%select(DT, TOTAL_COST)%>%mutate(group='Actual')
    sub2 = df3%>%select(DT, TOTAL_COST)%>%mutate(group='Predicted')
    # sub3 = data.frame(DT=as.Date('2022-12-24'), TOTAL_COST=NA, group='Actual') # add empty 12-24
    df_p = rbind(sub1, sub2)

    return(df_p)
}

plot_fee = function(df_p, ylab_text, legend_pos, y_inflat=1, x_text='12-10'){
    df_p$DT = as.factor(format(as.Date(df_p$DT), format = "%m-%d"))
    ymax = ceiling(median(df_p$TOTAL_COST, na.rm=T)/100)*y_inflat*100
    df_text = data.frame(DT=x_text, TOTAL_COST=ymax*0.95, group='Actual')
    p = ggplot(df_p, aes(x = DT, y = TOTAL_COST, fill=group)) + 
        geom_boxplot(outlier.color = NA) +
        ylim(0, ymax) +
        scale_x_discrete(breaks = c('12-01', '12-08', '12-15', '12-22', '12-29')) +
        geom_vline(xintercept='12-15', linetype='dashed', color='gray', size=1) + 
        geom_text(data=df_text, label=" Policy \n adjustment", vjust=0.5, hjust=0.3, size=3.5) +
        ylab(ylab_text) + xlab('') +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color="black"), 
            axis.title.y = element_text(size = 10), 
            panel.grid.major=element_blank(),panel.grid.minor=element_blank(), # remove grid
            legend.title=element_blank(), legend.position = c(legend_pos[1], legend_pos[2])) +
        scale_fill_nejm()
    return(p)
}

sort(table(inpat1$DPT_NAME), decreasing=T)

group1 = 'All clinical department'
group2 = 'Respiratory / Infectious'

ylab1 = sprintf('Hospitalization expense \n %s', group1)
ylab2 = sprintf('Hospitalization expense \n %s', group2)

fee1 = lm_predict_cost(inpat1, group1)
fee2 = lm_predict_cost(inpat1, group2)

p1 = plot_fee(fee1, ylab_text = ylab1, legend_pos=c(0.1, 0.8), y_inflat=5)
p2 = plot_fee(fee2, ylab_text = ylab2, legend_pos=c(0.1, 0.8), y_inflat=3.5)

p = ggarrange(p1, p2, ncol=1, nrow=2, common.legend=F, align = "v", hjust=0.1, vjust=0.1)

png('./plot/pred_fee.png',height=1000, width=1400, res=150)
print(p)
dev.off()



#====================================================================
# test
#====================================================================