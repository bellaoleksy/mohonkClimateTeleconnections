## Surface and deep water trend analysis

    #Using conceptual model to examine all possible drivers of summer surface temperature
    #Find interesting surface water variables from conceptual model
    AnnualData.SurfaceWater<-AnnualData%>%
      mutate(LengthMixedPeriod_days=ifelse(StartOfStratification_Day-IceOutDayofYear>0,
                                           StartOfStratification_Day-IceOutDayofYear,NA))%>%
      dplyr::select(SurfaceWaterTemp_Summer_degC,AirTemp_Summer_degC,SecchiDepth_Summer_m,
                    SurfaceWaterTemp_SpringMixed_degC,StartOfStratification_Day,
                    LengthMixedPeriod_days,Precip_Summer_mm)
    #Correlation matrix
    corr.matrix.surface<-cor(AnnualData.SurfaceWater,use="pairwise.complete.obs",method="spearman")
    #Has to be run after running the function
    corr.test.surface<-cor.mtest(as.matrix(AnnualData.SurfaceWater))
    #Print out correlations
    surface.results<-data.frame(r=corr.matrix.surface[,1],p=corr.test.surface[[1]][,1])
    surface.results$significance<-ifelse(p.adjust(surface.results$p,method = "bonferroni")<0.05,"*","NS")
    
    #pivot to long format
    SummerSurfaceTemperature.long<-AnnualData.SurfaceWater%>%
      pivot_longer(-SurfaceWaterTemp_Summer_degC,
                   names_to = "variable",
                   values_to = "value")
    #gpplot vs. values
    SummerSurfaceTemperature.long %>%
      ggplot(aes(x=value,y=SurfaceWaterTemp_Summer_degC))+
      geom_point()+
      facet_wrap(~variable,scales="free")+
      theme_bw()
    
    #Run PCA for surface temp variables ----------------------------------------
    #Drop rows with NA in any column (this would drop 1997, 2014 and 2015)
    #Also remove surface water temp
    
    #Run PCA with only the correlated variables
    pca.surface <- prcomp(AnnualData.SurfaceWater%>%
                            dplyr::select(AirTemp_Summer_degC,SecchiDepth_Summer_m)%>%
                            drop_na(),
                  center = TRUE,
                  scale. = TRUE) 
    #Eigenvalues (ignore any less than 1, find cite)
    pca.surface$sdev^2
    #Proportional of variance in each component
    pca.surface$sdev^2*100/sum(pca.surface$sdev^2)
    #Print out pca results
    print(pca.surface)
    #Generate the loadings for each axis
    axes.surface<-as.data.frame(predict(pca.surface, 
                                newdata=AnnualData.SurfaceWater%>%
                                  dplyr::select(-SurfaceWaterTemp_Summer_degC)%>%
                                  drop_na()))
    
    axesPlusSurfaceWater<-cbind(axes.surface,AnnualData.SurfaceWater%>%
                                  drop_na()%>%
                                  dplyr::select(SurfaceWaterTemp_Summer_degC))
    
    #Length of the arrows based on rotation and scaled. They are just comparable to each other
    lam.surface <- (pca.surface$sdev[1]*sqrt(33))^0.6
    len.surface <- data.frame(arrow.length=t(t(pca.surface$rotation[, 1]) * lam.surface)*0.8)
    
    #Reorder by absolute value of arrow length  
    len.surface<-len.surface%>%
      mutate(var=rownames(len.surface))%>%
      arrange(desc(abs(arrow.length)))%>%
      mutate(y=seq(21,20.75,by=-0.25),
             x=ifelse(arrow.length>1,0,arrow.length))
    
    # Draw Figure S11A. --------------------------------------------------------------
    #Plot the PC1 (reversed) vs. surface water temp for summer with arrows from the principal components on there
    gg.surface.summer<-ggplot(data=axesPlusSurfaceWater,
                              aes(x=PC1,y=SurfaceWaterTemp_Summer_degC))+
      geom_point()+
      theme_bw()+
      geom_smooth(method = "lm", se = TRUE)+
      geom_segment(data=len.surface,aes(x=0,xend=arrow.length,y=y,yend=y),arrow = arrow(length = unit(0.6, "picas"),type="closed"),
                   color = "red")+
      annotate("text", x=len.surface$x[1],y=len.surface$y[1],parse=TRUE, label="Summer~air~temp.~' '",hjust=1,size=3)+
      annotate("text", x=len.surface$x[2],y=len.surface$y[2],parse=TRUE, label="Summer~secchi~' '",hjust=1,size=3)+
      xlab("Principal component 1 loading")+
      ylab(expression(atop("Summer surface water", paste("temperature ("~degree*"C)"))))+
      theme_MS()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position="none")
    print(gg.surface.summer)

    #Regression with the PC1
    summary(lm(data=axesPlusSurfaceWater,SurfaceWaterTemp_Summer_degC~PC1))
    
    
    
#Deep water cooling analysis
    #Using conceptual model to examine all possible drivers of summer surface temperature
    AnnualData.DeepWater<-AnnualData%>%
      mutate(LengthMixedPeriod_days=ifelse(StartOfStratification_Day-IceOutDayofYear>0,
                                           StartOfStratification_Day-IceOutDayofYear,NA))%>%
      dplyr::select(DeepWaterTemp_Summer_degC,SurfaceWaterTemp_Summer_degC,
                    IceOutDayofYear,SecchiDepth_Summer_m,DeepWaterTemp_SpringMixed_degC,
                    StartOfStratification_Day,LengthMixedPeriod_days,
                    DeepWaterTemp_SpringPostIce_degC)
    #Correlation matrix
    corr.matrix.deep<-cor(AnnualData.DeepWater,use="pairwise.complete.obs",method="spearman")
    
    #Has to be run after running the function
    corr.test.deep<-cor.mtest(as.matrix(AnnualData.DeepWater))
    
    #Print out correlations
    #!!!!Calculate significance with adjustment to number of comparisons
    deep.results<-data.frame(r=corr.matrix.deep[,1],p=corr.test.deep[[1]][,1])
    deep.results$significance<-c(NA,ifelse(p.adjust(deep.results$p[-1],method = "bonferroni")<0.05,"*","NS"))
    
    #pivot to long format
    DeepSurfaceTemperature.long<-AnnualData.DeepWater%>%
      pivot_longer(-DeepWaterTemp_Summer_degC,names_to = "variable", values_to = "value")
    
    #gpplot vs. values
    # ggplot(data=DeepSurfaceTemperature.long,aes(x=value,y=DeepWaterTemp_Summer_degC))+
    #   geom_point()+
    #   facet_wrap(~variable,scales="free")+
    #   theme_bw()

    #Run PCA for deep temp variables -------------------------------------------
    #Drop rows with NA in any column (this would drop 1997, 2014 and 2015)
    #Also remove surface water temp
    
    #Run PCA with all the predictor variables
    #Run PCA with only the correlated variables
    pca.deep <- prcomp(AnnualData.DeepWater%>%
                         dplyr::select(IceOutDayofYear,DeepWaterTemp_SpringMixed_degC,
                                       LengthMixedPeriod_days)%>%drop_na(),
                  center = TRUE,
                  scale. = TRUE) 
    #Eigenvalues (ignore any less than 1, find cite)
    pca.deep$sdev^2
    #Proportional of variance in each component
    pca.deep$sdev^2*100/sum(pca.deep$sdev^2)
    #Print out pca results
    print(pca.deep)
    plot(pca.deep, type = "l")
    #Generate the loadings for each axis
    axes.deep<-as.data.frame(predict(pca.deep, 
                                newdata=AnnualData.DeepWater%>%
                                  dplyr::select(IceOutDayofYear,DeepWaterTemp_SpringMixed_degC,
                                                StartOfStratification_Day,LengthMixedPeriod_days)%>%
                                  drop_na()))
    
    #Biplot
    biplot(pca.deep,choices=1:2,scale=0.5)

    axesPlusDeepWater<-cbind(axes.deep,AnnualData.DeepWater%>%
                               drop_na()%>%
                               dplyr::select(DeepWaterTemp_Summer_degC))
    
    #Here I am reversing the arrow and the PC1 
    #Length of the arrows based on rotation and scaled. They are just comparable to each other
    lam.deep <- (pca.deep$sdev[1]*sqrt(33))^0.6
    len.deep <- data.frame(arrow.length=t(t(pca.deep$rotation[, 1]) * lam.deep)*0.8)
    len.deep<-len.deep*-1
    #Reorder by absolute value of arrow length  
    len.deep<-len.deep%>%mutate(var=rownames(len.deep))%>%
      arrange(desc(abs(arrow.length)))%>%
      mutate(y=seq(5.3,4.8,by=-0.25),
             x=0,hjust=ifelse(arrow.length<1,0,1))

    # Draw Figure S11B ---------------------------------------------------------
    # Plot the PC1 (reversed) vs.  Deep Water Temp for summer with arrows from the principal components on there
    gg.deep.summer<-ggplot(data=axesPlusDeepWater,aes(x=PC1*-1,y=DeepWaterTemp_Summer_degC))+
      geom_point()+
      theme_bw()+
      geom_smooth(method = "lm", se = TRUE)+
      geom_segment(data=len.deep,aes(x=0,xend=arrow.length,y=y,yend=y),arrow = arrow(length = unit(0.6, "picas"),type="closed"),
                   color = "red")+
      annotate("text", x=len.deep$x[1],y=len.deep$y[1],parse=TRUE, label="Mixed~period~length~' '",hjust=len.deep$hjust[1],size=3)+
      annotate("text", x=len.deep$x[2],y=len.deep$y[2],parse=TRUE, label="' '~Ice~out~day",hjust=len.deep$hjust[2],size=3)+
      annotate("text", x=len.deep$x[3],y=len.deep$y[3],parse=TRUE, label="Mixed~deep~temp.~' '",hjust=len.deep$hjust[3],size=3)+
      xlab("Principal component 1 loading")+
      ylab(expression(atop("Summer deep water", paste("temperature ("~degree*"C)"))))+
      theme_MS()+
     theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position="none")
    print(gg.deep.summer)
    

    #Regression with the PC1
    axesPlusDeepWater<-axesPlusDeepWater%>%mutate(PC1.reversed=PC1*-1)
    summary(lm(data=axesPlusDeepWater,DeepWaterTemp_Summer_degC~PC1.reversed))
    
    #**Arrange into a two panel plot
    #Export Figure S10. PC1 versus deep or surface temperature
    #http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
    gg.2panel.pc<-ggarrange(gg.surface.summer+
                           theme(axis.title.x=element_blank(),
                                 axis.text.x=element_blank(),
                                 axis.ticks.x=element_blank(),
                                 plot.margin=unit(c(2,0.5,0,1), "lines"),
                                 axis.ticks.length.x = unit(0, "pt"))+
                           labs(x=NULL, title=NULL)+
                             scale_x_continuous(limits=c(-2.9,3)),
                         gg.deep.summer+
                           theme(plot.margin=unit(c(0,0.5,2,1), "lines"))+
                           scale_x_continuous(limits=c(-2.9,3)),
                         nrow=2)
    print(gg.2panel.pc)  
    
    #  >> Export 2 panel figure PC1 plot -------------------------------------------
    op <- par(no.readonly = TRUE)
    jpeg("figures/supplementary/figureS11.PanelvsPC1.jpg",
         width = 3.3, height = 5.2,
         units = "in",res = 300)
    
    gg.2panel.pc
    
    dev.off()  
    par(op)
    
    
    #Clean up environment
    rm(list = ls()[grep("gg.", ls())])
    rm(list = ls()[grep("axes", ls())])
    rm(list = ls()[grep("corr", ls())])
    rm(list = ls()[grep("len", ls())])
    rm(list = ls()[grep("pca", ls())])
    rm(list = ls()[grep(".long", ls())])
    rm(list = ls()[grep("AnnualData.", ls())])
    rm(list = ls()[grep(".results", ls())])
    rm(op)
    