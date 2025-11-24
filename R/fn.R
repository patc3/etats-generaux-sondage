#### imports ####
library(tidyverse)
library(gentleman)
library(openxlsx)



#### helpers ####
unun<-\(l)l |> unlist() |> unname()
tab<-\(...)table(...,useNA="always")



#### vars ####
CPATH<-"files/recode.xlsx"
RPATH<-"files/results.xlsx"



#### load data ####
load_data<-function()
{
  
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



