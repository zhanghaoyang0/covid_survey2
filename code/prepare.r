
#====================================================================
# load packages and defind functions
#====================================================================
libs = c('dplyr', 'openxlsx', 'stringr', 'pracma', 'xts', 'ggplot2', 'ggsci', 'ggpubr', 'scales')
lapply(libs, require, character.only = TRUE)
options(stringsAsFactors=F)

# study period
days = seq(as.Date('2022/11/01'), by = "day", length.out = 61)
adjust_day = as.Date('2022-12-10')

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

# find top disease, e.g., find_topDis(inpat, 'PAT_DIA_NAME', 11)
clean_dis = function(vec){
    vec = gsub('急性|慢性', '', vec)
    vec[grepl('肺炎', vec)] = '肺炎'; vec[grepl('支气管炎', vec)] = '支气管炎'; vec[grepl('上呼吸道感染', vec)] = '上呼吸道感染'
    vec[grepl('化学治疗|健康查体|咨询', vec)] = 'other' # drop this 
    return(vec)
}

find_topDis = function(vec, topn){
    vec = sort(table(vec), decreasing=T)[1:topn]
    cbind(names(vec[1:topn]))
    return(t(t(vec)))
}

chn_to_eng = function(vec, dict, notFoundAsOther=T){
    for (i in 1:nrow(dict)){
        key = dict[i, 'key']; val = dict[i, 'value']
        vec[vec==key] = val
    }
    if (notFoundAsOther==T){vec[!vec%in%dict$value] = 'Other'}
    return(vec)
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


# plot ts
# covid pat appear after 1210

# title1 = bquote(italic("N")['handled inpatient']~' per week')

#====================================================================
# prop of dis
#====================================================================





plot_disPie = function(df, policy_, title){
    df_p = df%>%filter(policy==policy_)
    df_p = df_p%>%mutate(ypos = cumsum(prop)- 0.5*prop)
    p = ggplot(df_p, aes(x="", y=prop, fill=dis)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        theme_void() + 
        ggtitle(title) +
        geom_text(aes(x = 1.6, label = scales::percent(prop, accuracy = .1)), position = position_stack(vjust = 0.5), size=3) +
        guides(fill=guide_legend(title="Disease", ncol = 2)) + # legend row
        scale_fill_nejm()
    return(p)
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


#====================================================================
# for analysis
#====================================================================
