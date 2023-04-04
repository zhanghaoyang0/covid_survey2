
#====================================================================
# load packages and defind functions
#====================================================================
libs = c('dplyr', 'openxlsx', 'stringr', 'ggplot2', 'ggsci', 'ggpubr')
lapply(libs, require, character.only = TRUE) 
options(stringsAsFactors=F)

# study period
days = seq(as.Date('2022/11/01'), by = "day", length.out = 61)

# get items with matched pattern
# e.g, get('a', c('a1', 'a', 'c'))
get = function(key, vector, exact=F, v=F){
    if (exact==T){out =vector[grep(key, vector, invert=v)]}
    else {out = vector[grep(toupper(key), toupper(vector), invert=v)]}
    return(out)
}

# convert character vector to numeric 
# e.g, df = df%>%mutate_if(is_numeric,as.numeric)
is_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}

# find col with all na
not_all_na <- function(x) any(!is.na(x))

# clean df
clean_df = function(df){
    df[df==''] = NA
    df1 = df%>%select(where(not_all_na))%>%filter(if_any(everything(), ~ !is.na(.)))
    print(paste0('remove ', ncol(df)-ncol(df1), ' cols with all na'))
    print(paste0('remove ', nrow(df)-nrow(df1), ' rows with all na'))  
    drop_cols = c()
    for (i in 1:ncol(df1)){
        tab = table(df1[,i])
        miss_prop = sum(is.na(df1[,i]))/nrow(df1)
        if (tab[which.max(tab)]/sum(tab)>0.95 | miss_prop>0.9 ){
            drop_cols = c(drop_cols, i)
        }
    }
    df1[, drop_cols] = NULL
    print(paste0('remove ', length(drop_cols), ' cols which are dominant or have too much na'))
    return(df1)
}

# clean outpat covid epi history
clean_PRES_ILLN = function(col){
    col = gsub('\\（.*?\\）', '', col) # remove string in brackets, use ? to least match
    col = gsub('其|抗原|新冠|新冠病毒|调查|
        ①|②|④|⑤|③|①|㎜|1.|2.|3.|\n|、|（|）|[()]|/|：|-|:| ', '', col)
    col = gsub('陪客|父|母|家属|婆|爸|妈|公|爷|奶|姐|妹|弟|哥|姑|姥|同学|家人|保姆|月嫂|叔|家中|家长|家庭|同居者|多人|大人|有人|同住|学校', 
        'close', col)
    col = gsub('[否认|无][\U4E00-\U9FFF|0-9]+[史|者|病]', '', col)
    col = gsub('无阳性', '阴性', col)
    cols = str_split_fixed(col, '流行病学调查史|流行病学史', 2)
    ## epi
    epi = cols[,2]
    epi[!grepl('阳性', epi)] = 'nega'
    # unique(epi[!grepl('close', epi)]) # check if any close is missing
    epi[grepl('患儿[\U4E00-\U9FFF|0-9]*阳性|患儿及[\U4E00-\U9FFF|0-9|close]*阳性|close及患儿[\U4E00-\U9FFF|0-9]*阳性|全家阳性', epi)] = 'posi' # if not close between, 患儿and阳性, it is positive
    epi[grepl('患儿[\U4E00-\U9FFF|0-9]+阴性', epi)] = 'nega'
    # unique(epi)
    # drop clause without postive info 
    for (i in 1:length(epi)){
        item = epi[i]
        if(item%in%c('nega', 'posi')){next}
        items = strsplit(item, '。|；|，')[[1]]
        items = items[grepl('阳性', items)]
        items = paste0(items, collapse=';')
        epi[i] = items
    }
    # unique(epi)
    # unique(epi[grepl('close', epi)])
    epi[grepl('否认close中阳性', epi)] = 'nega'
    epi[grepl('close|有阳性患者接触', epi)] = 'contact_posi'
    epi[!epi%in%c('nega', 'contact_posi')] = 'posi'
    return(epi)
}



## get duplicate value, e.g, get_dup(c(1,2,1,NA,NA))
get_dup = function(x, keepNA=F){
    x = x[duplicated(x)]
    if (keepNA==F){x=x[!is.na(x)]}
    return(x)
}




#====================================================================
# for nvist
#====================================================================
# get number of visit by group
get_nvisit_bygroup = function(df, date_col, group_col, dates, groups){
    out = c()
    for (day in dates){
        sub = df[df[,date_col] == day,]
        for (group in groups){
            if (group=='All'){num = nrow(sub)}
            else if (group=='Other'){num = sum(!sub[,group_col]%in%groups)}
            else if (group=='COVID positive'){num = sum(sub$epi=='posi')}
            else if (group=='COVID contact history'){num = sum(sub$epi=='contact_posi')}
            else {num = sum(sub[,group_col]==group)}
            out = c(out, day, group, num)
        }
    }
    nvisit = data.frame(matrix(out, ncol=3, byrow=T))%>%rename(day=X1, group=X2, num=X3)%>%
        mutate_if(is_numeric,as.numeric)%>%mutate(day=as.Date(day,origin="1970-01-01"))
    return(nvisit)
}
# plot ts
plot_nvist = function(df, groups){
    plots = list()
    for (i in 1:length(groups)){
        group = groups[[i]]
        df_p = df%>%filter(group%in%groups[[i]])
        ymax = ceiling(max(df_p$num)/100)*120
        df_text = df_p%>%filter(day==as.Date('2022-12-07'))%>%filter(num==max(num))%>%unique()%>%mutate(num=ymax*0.9)
        p = ggplot(df_p, aes(x=day, y=num, group=group)) +
            geom_point(aes(color=group)) + geom_line(aes(color=group)) + 
            geom_vline(xintercept=as.Date('2022-12-07'), linetype='dashed', color='blue', size=1.5) +        
            labs(x=("Date"), y=("Number of patients")) + ylim(0, ymax) +
            scale_x_date(date_breaks = "weeks" , date_labels = "%m-%d") +
            geom_text(data=df_text, label="After adjustment \nof zero-COVID policy*", vjust=0, hjust=-0.1, size=3) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color="black"))+
            theme(legend.title=element_blank()) +
            scale_fill_nejm()
        plots[[i]] = p
    }
    return(plots)
}


#====================================================================
# for clean of staff data 
#====================================================================
# fix date in staff_posi, e.g., fix_date("1.01"); fix_date("12.1"); fix_date(NA)
fix_date = function(x){
    if (is.na(x)){out = x} else if (grepl('1.', x, fixed=T)) {
    out = paste0('2023.', x)
    } else {out = paste0('2022.', x)}
    out = as.Date(out, format="%Y.%m.%d")
    return(out)
}