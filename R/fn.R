#### imports ####
library(tidyverse)
library(gentleman)
library(openxlsx)
library(tidytext)
library(dplyr)
library(stopwords)
library(wordcloud)
library(RColorBrewer)


#### helpers ####
unun<-\(l)l |> unlist() |> unname()
tab<-\(...)table(...,useNA="always")



#### vars ####
CPATH<-"files/recode.xlsx"
RPATH<-"files/results.xlsx"
DPATH<-"data/États+généraux+(VDR)_24+novembre+2025_13.30.sav"


VARS<-list(
  txt=c("defis_recherche_"%p%1:3,
        "enjeux_"%p%1:3,
        "idees",
        "commentaire"),
  num=c("defis_financement_"%p%1:6,
        "enjeux_interdis",
        "interet_1")
)


#### load data ####
load_data<-function()
{
  haven::read_spss(DPATH)
}



#### data prep ####




#### export ####
export_csv<-function(df, fname=NULL, Mplus=FALSE)
{
  if(is.null(fname)) fname<-df |> substitute() |> deparse()
  fpath<-(if(Mplus)"Mplus/"else"data/")%p%fname%p%".csv"
  message("Outputting to"%P%fpath)
  if(!Mplus)
    df |> write.csv(fpath,
                    na="",
                    row.names = F)
  if(Mplus)
  {
    dffv<-df |> select(!where(is.character))
    dffv |> write.table(fpath,
                        na="999",
                        row.names = F,
                        dec=".",
                        sep=",",
                        col.names=FALSE)
    dffv |> 
      names() |> 
      writeLines(con = "Mplus/colnames_"%p%fname%p%".txt")
  }
}


export_xl<-function(df,fname)
{
  df |> writexl::write_xlsx("data/"%p%fname%p%".xlsx")
}


add_sheet_to_results <- function(tbl,sheet)
{
  fpath<-RPATH
  res<-loadWorkbook(fpath)
  addWorksheet(res,sheet)
  writeDataTable(wb=res,
				 sheet=sheet,
				 x=tbl)
  res |> saveWorkbook(fpath, overwrite = T)
}


#### descriptives ####
get_correlogram<-function(df,vars=NULL,method="pearson",lab=TRUE)
{
  if(is.null(vars))vars<-colnames(df)
  df |> 
	select(all_of(vars)) |> 
	psych::corr.test(use="pair", adjust="none", method=method) |> 
	(\(cor) ggcorrplot::ggcorrplot(corr=cor$r, 
								   p.mat=cor$p,
								   lab=lab))()
}

##### missing data #####
# https://cran.r-project.org/web/packages/VIM/vignettes/VIM.html
plot_missing<-function(df,vars=NULL)
{
  if(is.null(vars))vars<-colnames(df)
  df |> 
	select(all_of(vars)) |> 
	VIM::aggr(plot = FALSE) |> 
	plot(numbers = TRUE, 
		 prop = FALSE, 
		 varheight=T, 
		 combined=T,
		 cex.lab=1.2,
		 cex.axis=c(0.48),
		 cex.numbers=1.2)
}


#### models ####



