##############################################################################
#
#Plot Multi Acmo files to boxplot and cdfplot
#
#Created by Wei Xiong, University of Florida.  3/29/2016
#User Guide, if you don't want to make changes in the script, please oraginze your data as the following steps
#1. Put this script in a directory and indicate it at inputDir
#2. Place all you acmo files in the directory
#3. Execut the script, plots are saved in as a pdf file with the name that you defined in pngoutput.
#############################################################################

#################################################################################################################
#<<<<<<<<<<<<<<<<<<<<<<<<<<< YOU NEED CHANGE ONLY IN HERE >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#################################################################################################################
plottype<-"b" # "c"    # Type of the plots: Box/whisker (b), CDF, cumulative distribution (c)   <<<<<<<<<<<<  Change here
yieldorratio<-"y"  # "r" #Ploting absolute yield (y) or change ratio (r) <<<<<<<<< Change here
#The directory that you put you script and acmo files
inputdir<-"/Users/weixiong/Development/face-it/RIA/ria_standardplots/1.0.5_either input zip or csv/3/CM1_CM2_Plots/CM1_CM2_Plots"# You directory that put you results and script.  <<<<<<<<<<<<<< Change here
#The name for you plot
pngoutput<-"Ratio.pdf" # Name of the plot output file, Ratio.paf or Yield.pdf   <<<<<<<<<<<  Change here
#############################################################################################################


#Don't need make change after this line

name_unit<-function(inputcode){
    name<-c("ID","Name of experiment", "Field Overlay","Seaonal Strategy","Rotational Analysis","","Treatment Name","Climate ID code","Climate replication number",	"Region ID","Regional stratum identification number","RAP ID", "Management regimen ID","Names of institutions","Crop rotation", "Weather station ID","Soil ID", "Site Latitude", "Site Longitude",	"Crop type", "Crop model-specific cultivar ID", "Cultivar name", "Start of simulation date",	"Planting date","Observed harvested yield, dry weight", "Observed total above-ground biomass at harvest",	"Observed harvest date",	"Total number of irrigation events",	"Total amount of irrigation",	"Type of irrigation application",	"Total number of fertilizer applications",	"Total N applied",	"Total P applied",	"Total K applied",	"Manure and applied oganic matter",	"Total number of tillage applications",	"Tillage type (hand, animal or mechanized)",	"Experiment ID",	"Weather ID",	"Soil ID",	"DOME ID for Overlay",	"DOME ID for Seasonal",  "DOME ID for Rotational", "Short name of crop model used for simulations",	"Model name and version number", "Simulated harvest yield, dry matter", "Simulated above-ground biomass at harvest, dry matter",	"Simulated anthesis date",	"Simulated maturity date",	"Simulated harvest date",	"Simulated leaf area index, maximum",	"Total precipitation from planting to harvest",	"Simulated evapotranspiration, planting to harvest",	"Simulated N uptake during season", "Simulated N leached up to harvest maturity")
    unit<-c("text",	"text",	"text",	"text",	"text",	"number",	"text",	"code",	"number",	"code",	"number",	"code",	"code",	"text",	"number",	"text",	"text",	"decimal degrees",	"decimal degrees",	"text",	"text",	"text",	"yyyy-mm-dd",	"yyyy-mm-dd",	"kg/ha",	"kg/ha",	"yyyy-mm-dd",	"number",	"mm",	"text",	"number",	"kg[N]/ha",	"kg[P]/ha",	"kg[K]/ha",	"kg/ha",	"#",	"text",	"text",	"text",	"text",	"text",	"text",	"text",	"text",	"text",	"kg/ha",	"kg/ha",	"das",	"das",	"das",	"m2/m2",	"mm",	"mm",	"kg/ha",	"kg/ha")
    code<-c("SUITE_ID",	"EXNAME",	"FIELD_OVERLAY",	"SEASONAL_STRATEGY",	"ROTATIONAL_ANALYSIS",	"RUN#",	"TRT_NAME",	"CLIM_ID",	"CLIM_REP",	"REG_ID",	"STRATUM",	"RAP_ID",	"MAN_ID",	"INSTITUTION",	"ROTATION",	"WST_ID",	"SOIL_ID",	"FL_LAT",	"FL_LONG",	"CRID_text",	"CUL_ID",	"CUL_NAME",	"SDAT",	"PDATE",	"HWAH",	"CWAH",	"HDATE",	"IR#C",	"IR_TOT",	"IROP_text",	"FE_#",	"FEN_TOT",	"FEP_TOT",	"FEK_TOT",	"OM_TOT","TI_#",	"TIIMP_text",	"EID",	"WID",	"SID",	"DOID",	"DSID",	"DRID",	"CROP_MODEL",	"MODEL_VER",	"HWAH_S",	"CWAH_S",	"ADAT_S",	"MDAT_S",	"HADAT_S",	"LAIX_S",	"PRCP_S",	"ETCP_S",	"NUCM_S",	"NLCM_S")
    for (thisi in 1:length(code)) {
        if (inputcode==code[thisi]) {
            all<-paste(name[thisi],"(",unit[thisi],")")
            break
        }
    }
    return(all)
}

plots<-function(title,plottype,plotformat,plotvariable,yieldorratio,inputdir,group,count,pngoutput,output_acmo){
    #acmoinput,title_or_group,color,plottype,plotvariable,output
    #len<-length(acmoinput)
    if (yieldorratio=="r") {
    for (thiscount in 1:count){
        if (count==1) csvfiles<-list.files(inputdir,pattern="*APSIM.*csv$",ignore.case=TRUE)
        if (count==2) csvfiles<-list.files(inputdir,pattern="*DSSAT.*csv$",ignore.case=TRUE)
    for (thisinput in 1:length(csvfiles)){
        if (thisinput==1){
            openfile<-paste(inputdir,"/",csvfiles[thisinput],sep="")
            cat(readLines(openfile),file=paste(inputdir,"/",output_acmo,sep=""),sep="\n")
            oridata<-read.csv(openfile,header=T,skip=2,sep=",")
            df_1<-data.frame(CROP_MODEL=oridata$CROP_MODEL,CLIM_ID=oridata$CLIM_ID,GROUP=oridata[group],VALUE=oridata[plotvariable])
            df_1_0<-aggregate(HWAH_S~EID,df_1,mean)
            df_1_0<-data.frame(CROP_MODEL=oridata$CROP_MODEL[1],CLIM_ID=oridata$CLIM_ID[1],EID=df_1_0$EID,HWAH_S=df_1_0$HWAH_S)
            #df_1_0$CLIM_ID<-sub("^([^.]*).*","\\1",csvfiles[thisinput])
        }else{
            openfile<-paste(inputdir,"/",csvfiles[thisinput],sep="")
            temp<-readLines(openfile)
            for (i in 1:3) temp<-temp[-1]
            cat(temp,file=paste(inputdir,"/",output_acmo,sep=""),sep="\n",append=TRUE)
            oridata<-read.csv(openfile,header=T,skip=2,sep=",")
            dftemp<-data.frame(CROP_MODEL=oridata$CROP_MODEL,CLIM_ID=oridata$CLIM_ID,GROUP=oridata[group],VALUE=oridata[plotvariable])
            dftemp0<-aggregate(HWAH_S~EID,dftemp,mean)
            #HWAH_S=(dftemp0$HWAH_S-df_1_0$HWAH_S)*100/df_1_0$HWAH_S
            dftemp<-data.frame(CROP_MODEL=oridata$CROP_MODEL[1],CLIM_ID=oridata$CLIM_ID[1],EID=dftemp0$EID,HWAH_S=(dftemp0$HWAH_S-df_1_0$HWAH_S)/df_1_0$HWAH_S)
            #dftemp$CLIM_ID<-sub("^([^.]*).*","\\1",csvfiles[thisinput])
            if (thiscount==1 && thisinput==2) {
                df<-dftemp
            }else{
                df<-rbind(df,dftemp)
            }
        }
    }
    }
    ylabname<-"Yield Change Ratio"
    }
    if (yieldorratio=="y"){
        for (thiscount in 1:count){
            if (count==1) csvfiles<-list.files(inputdir,pattern="*APSIM.*csv$",ignore.case=TRUE)
            if (count==2) csvfiles<-list.files(inputdir,pattern="*DSSAT.*csv$",ignore.case=TRUE)
            for (thisinput in 1:length(csvfiles)){
                if (thisinput==1 && thiscount==1){
                    openfile<-paste(inputdir,"/",csvfiles[thisinput],sep="")
                    cat(readLines(openfile),file=paste(inputdir,"/",output_acmo,sep=""),sep="\n")
                    oridata<-read.csv(openfile,header=T,skip=2,sep=",")
                    df<-data.frame(CROP_MODEL=oridata$CROP_MODEL,CLIM_ID=oridata$CLIM_ID,GROUP=oridata[group],VALUE=oridata[plotvariable])
                    df<-aggregate(HWAH_S~EID,df,mean)
                    df<-data.frame(CROP_MODEL=oridata$CROP_MODEL[1],CLIM_ID=oridata$CLIM_ID[1],EID=df$EID,HWAH_S=df$HWAH_S)
                    #df$CLIM_ID<-sub("^([^.]*).*","\\1",csvfiles[thisinput])
                }else{
                    openfile<-paste(inputdir,"/",csvfiles[thisinput],sep="")
                    temp<-readLines(openfile)
                    for (i in 1:3) temp<-temp[-1]
                    cat(temp,file=paste(inputdir,"/",output_acmo,sep=""),sep="\n",append=TRUE)
                    oridata<-read.csv(openfile,header=T,skip=2,sep=",")
                    dftemp<-data.frame(CROP_MODEL=oridata$CROP_MODEL,CLIM_ID=oridata$CLIM_ID,GROUP=oridata[group],VALUE=oridata[plotvariable])
                    dftemp<-aggregate(HWAH_S~EID,dftemp,mean)
                    dftemp<-data.frame(CROP_MODEL=oridata$CROP_MODEL[1],CLIM_ID=oridata$CLIM_ID[1],EID=dftemp$EID,HWAH_S=dftemp$HWAH_S)
                    #dftemp$CLIM_ID<-sub("^([^.]*).*","\\1",csvfiles[thisinput])
                    df<-rbind(df,dftemp)
                }
            }
        }
        ylabname<-name_unit(plotvariable)
    }
    colnames(df)<-c("CROP_MODEL","CLIM_ID","GROUP","VALUE")
    #temp<-NA
    title<-paste(title, " of KURNOOL",oridata$REG_ID[1],sep="")
    
    #Name_Model<-unique(df$CROP_MODEL)
    #Name_Clim<-unique(df$CLIM_ID)
    #if (length(Name_Model)==1) tick<-c(1:length(Name_Clim))
    #if (length(Name_Model)==2) tick<-c(1:length(Name_Clim),seq(length(Name_Clim)+2,length(Name_Clim)*2+2,1))
    #if (length(Name_Model)==3) tick<-c(1:length(Name_Clim),seq(length(Name_Clim)+2,length(Name_Clim)*2+2,1),seq(length(Name_Clim)*2+4,length(Name_Clim)*3+4,1))
    
    #if (plotformat=="png") png(pngoutput)#,width=850,height=500)
    #if (plotformat=="pdf") pdf(pngoutput)#,width=9,height=5)
    #if (plotformat=="tiff") tiff(pngoutput)#,width=850,height=500)
    #if (plotformat=="jpeg") jpeg(pngoutput)#,width=850,height=500)
    pdf(paste(inputdir,"/",pngoutput,sep=""), width=9,height=5)
    library(ggplot2)
   
    if (plottype=="b") {
        
        df$CLIM_ID<-factor(df$CLIM_ID)
        p<-ggplot(df,aes(x=CLIM_ID,y=VALUE,color=CROP_MODEL)) +
           geom_boxplot(outlier.shape=NA) +
           xlab("GCMs") +
           ylab(ylabname) +
           ggtitle(title)
        print(p)
        
    }
    if (plottype=="c") {
         df$CROP_MODEL<-factor(df$CROP_MODEL)
         p<-ggplot(df,aes(VALUE,color=CLIM_ID))+
            stat_ecdf()+
            facet_wrap(~CROP_MODEL)+
            xlab(name_unit(plotvariable))+
            ylab("Cumulative Frequency")
            ggtitle(title)
         print(p)
    }
    
    dev.off()

}
options(echo=TRUE)
args<-commandArgs(trailingOnly=TRUE)
#print(args)

###############################Here is the place that you only need to make change##########################################################
title<-"Result"  # Title of the plots
plotformat<-"pdf" # No need to change
plotvariable<-"HWAH_S" # Plot variables, no need to change
group<-"EID" # Here use farmer ID, no need to change
count<-"2"  # How many model results, here we have APSIM and DSSAT, give it value 2
output_acmo<-"1.csv"   # A Acmo.csv file that combining all input acmo.csv into one
###########################################################################################################################################




plots(title,plottype,plotformat,plotvariable,yieldorratio,inputdir,group,count,pngoutput,output_acmo)


