
prepare <- function() {
  if (!dir.exists(('output'))) {
    dir.create('output', recursive = TRUE)
  }
  
  if (!('metafor' %in% installed.packages()[,1])) {
    print('not installed')
    install.packages('metafor')
  }
}

read_data <- function() {
  # 데이터 불러오기
  meta_data = read.csv("data.csv")
  # 데이터 확인하기
  # print(head(meta_data))
  meta_data
}

work_indep <- function(meta_data) {
  # 독립변인의 종류에 따른 효과크기의 수 저장
  n_indep = table(meta_data$x)
  
  # 독립변인의 종류에 따른 효과크기의 수 확인하기
  # print(n_indep)
  write.csv(n_indep, "output/result.csv")
  
  
  # 효과크기의 수가 3개 이상인 것만
  indep_vars = names(n_indep[n_indep>=3])
  
  # 추출된 변수 확인하기 
  # print(indep_vars)
  write.csv(indep_vars, "output/over3.csv")
  
}

work_meta_analysis <- function(meta_data) {
  ## 메타분석 패키지 불러오기
  library(metafor)
  
  # 독립변인 선택
  partial_data = subset(meta_data, x=="진로결정 자기효능감")
  #print(partial_data)
  
  # 메타분석
  meta_obj = rma(ri=Correlation, ni=N, data=partial_data, measure="ZCOR")
  #print(meta_obj)
  
  # 평균상관계수
  meta_cor = predict(meta_obj, transf=transf.ztor)
  print("평균상관계수")
  print(meta_cor)
  
  ## 메타분석 효과크기 그림
  png(filename="output/meta1.png")
  forest(meta_obj, transf=transf.ztor)
  dev.off()
  print("export : 메타분석 효과크기 그림")
  
  ## 연구명
  study_name = paste0(partial_data$저자, " (", partial_data$출판년도, ")")
  
  ## 연구명 넣고 다시 그리기
  png(filename="output/meta2.png")
  forest(meta_obj, transf=transf.ztor, slab=study_name)
  dev.off()
  print("export : 메타분석 효과크기 그림 (연구명 포함)")
  
  # for investigate publication bias
  png(filename="output/meta3.png")
  funnel(meta_obj)
  dev.off()
  #print(regtest(meta_obj))
  print("export : investigate publication bias")
  
  cor_data_rf = trimfill(meta_obj)
  png(filename="output/meta4.png")
  funnel(cor_data_rf)
  dev.off()
  print("export : investigate publication bias (trimfill)")
  
}