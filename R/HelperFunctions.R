#' clean_PullAllocations
#'
#' This function updates the headers from the main PullAllocations function
#' @param df output of pull allocations
#' @param activepage if we are in RAD change things a little from program costing
#' @export
#' @examples
#' clean_PullAllocations(df,activepage=input$activepage)
#'
clean_PullAllocations<-function(df,activepage=input$activepage){
          df[,'Cost/Position']<-paste('(',df[,'Obj1Code'],') ',df[,'Obj1'],sep='')
          df[,'PercentAppliedToProg']<-df[,'PercentAppliedToProg']*100

          include<-NULL
          if(activepage=='RAD'){
           include<-c('Quartile','ProgGroup','PBBComment')
          }
          #browser()
          df<-df[c('ItemMeta1','Cost Type','Obj1','Cost/Position','PercentAppliedToProg','NumberOfItems','TotalCost','ItemID','ProgID',
                   'ProgNum','ProgName','AllocComments','AllocLastUpdated','AllocLastUser','AcctType','byFTE','byFTEDivision','NameMeta',
                   'Fund','AcctCode','obj_level_01','obj_level_02','obj_level_03','obj_level_04','Obj2','Obj3','Obj4',
                   'Div1','Div2','Div3','Div4','Div5','Div6','Div7','Div8','ServiceType','RXCommentID','RXCommentID_cp',include)]

          colnames(df)<-c('User Group','Cost Type','Cost/Position','ID','Allocation','FTE','Cost','ItemID','ProgID',
                          'ProgNum','ProgName','Comments','LastUpdated','LastUser','AcctType','byFTE','byFTEDivision','Name','Fund',
                          'Account','obj_level_01','obj_level_02','obj_level_03','obj_level_04','Obj2','Obj3','Obj4','Div1','Div2',
                          'Div3','Div4','Div5','Div6','Div7','Div8','ServiceType','RXCommentID','RXCommentID_cp',include)

          df[,'Obj1']<-df[,'Cost/Position']

          NonPersonnel.expense<-df[df$`Cost Type`=='NonPersonnel',]
          NonPersonnel.expense<-NonPersonnel.expense[NonPersonnel.expense$AcctType=='Expense',]
          if(nrow(NonPersonnel.expense)>0){NonPersonnel.expense<-NonPersonnel.expense}else{NonPersonnel.expense<-NULL}

          NonPersonnel.revenue<-df[df$`Cost Type`=='NonPersonnel',]
          NonPersonnel.revenue<-NonPersonnel.revenue[NonPersonnel.revenue$AcctType=='Revenue',]
          if(nrow(NonPersonnel.revenue)>0){NonPersonnel.revenue[,'Cost Type']<-'Revenue'}else{NonPersonnel.revenue}

          Personnel<-df[df$`Cost Type`=='Personnel',]
          if(nrow(Personnel)>0){Personnel<-Personnel}else{Personnel<-NULL}


          df<-rbind(Personnel,NonPersonnel.expense,NonPersonnel.revenue)

          return(df)

}

#' update_drillstack_selectInput
#'
#' This function updates the selectInputs in the modal of update stack level stacking
#' @param session the session variable used in shiny::updateSelectInput which is called here
#' @param inputId id of selectInput element
#' @param choices choices for drop down. function will make the Div's speficic to the clients naming conventions
#' @param selected What value to select
#' @param CostModelInfo Table to use for the naming conventions in the account set-up
#' @export
#' @examples
#' update_drillstack_selectInput(session,inputId,choices,selected,CostModelInfo=values.setup$CostModelInfo)
#'

update_drillstack_selectInput<-function(session,inputId,choices,selected,CostModelInfo=values.setup$CostModelInfo){
          #updates choices to the org's div level names and removes unused divisions
          choices<-c('Not Used',choices)

          names(choices)<-choices

          names(choices)[names(choices)=='Div1']<-CostModelInfo$Div1Name
          names(choices)[names(choices)=='Div2']<-CostModelInfo$Div2Name
          names(choices)[names(choices)=='Div3']<-CostModelInfo$Div3Name
          names(choices)[names(choices)=='Div4']<-CostModelInfo$Div4Name
          names(choices)[names(choices)=='Div5']<-CostModelInfo$Div5Name
          names(choices)[names(choices)=='Div6']<-CostModelInfo$Div6Name
          names(choices)[names(choices)=='Div7']<-CostModelInfo$Div7Name
          names(choices)[names(choices)=='Div8']<-CostModelInfo$Div8Name

          names.to.keep<-names(choices)[which(!is.element(names(choices),c('Div1','Div2','Div3','Div4','Div5','Div6','Div7','Div8')))]

          choices<-choices[names.to.keep]

          updateSelectInput(session, inputId, label = NULL, choices = choices,
          selected = selected)


}


#' update_drillstack_selectInput_set
#'
#' This function updates the block of selectInputs in the modal of update stack level stacking.
#' @param session the session variable used in shiny::updateSelectInput which is called here
#' @param activepage if we are in RAD change things a little from program costing
#' @export
#' @examples
#' update_drillstack_selectInput_set(session,activepage=input$activepage)

update_drillstack_selectInput_set<-function(session,activepage=input$activepage,CostModelInfo=values.setup$CostModelInfo){

         if(activepage=='RAD'){
            choices=c('User Group','ProgName','Quartile','ProgGroup','PBBComment','Cost Type', 'Fund', 'Div1','Div2','Div3','Div4','Div5','Div6','Div7','Div8', 'obj_level_01','obj_level_02','obj_level_03','obj_level_04')
         }

         if(activepage=='PRG_COSTING'){
            choices=c('User Group','ProgName','Cost Type', 'Fund', 'Div1','Div2','Div3','Div4','Div5','Div6','Div7','Div8', 'obj_level_01','obj_level_02','obj_level_03','obj_level_04')
         }

          update_drillstack_selectInput(session,inputId='program_level1',choices=choices,selected='ProgName',CostModelInfo)
          update_drillstack_selectInput(session,inputId='program_level2',choices=choices,selected='Cost Type',CostModelInfo)
          update_drillstack_selectInput(session,inputId='program_level3',choices=choices,selected='obj_level_01',CostModelInfo)
          update_drillstack_selectInput(session,inputId='program_level4',choices=choices,selected='Not Used',CostModelInfo)
          update_drillstack_selectInput(session,inputId='program_level5',choices=choices,selected='Not Used',CostModelInfo)

          choices=c('User Group','Cost Type', 'Fund', 'Div1','Div2','Div3','Div4','Div5','Div6','Div7','Div8', 'obj_level_01','obj_level_02','obj_level_03','obj_level_04')

          update_drillstack_selectInput(session,inputId='alloc_level1',choices=choices,selected='Cost Type',CostModelInfo)
          update_drillstack_selectInput(session,inputId='alloc_level2',choices=choices,selected='obj_level_01',CostModelInfo)
          update_drillstack_selectInput(session,inputId='alloc_level3',choices=choices,selected='obj_level_02',CostModelInfo)
          update_drillstack_selectInput(session,inputId='alloc_level4',choices=choices,selected='Not Used',CostModelInfo)
          update_drillstack_selectInput(session,inputId='alloc_level5',choices=choices,selected='Not Used',CostModelInfo)

}
