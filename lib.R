
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
  # ������ �ҷ�����
  meta_data = read.csv("data.csv")
  # ������ Ȯ���ϱ�
  # print(head(meta_data))
  meta_data
}

work_indep <- function(meta_data) {
  # ���������� ������ ���� ȿ��ũ���� �� ����
  n_indep = table(meta_data$x)
  
  # ���������� ������ ���� ȿ��ũ���� �� Ȯ���ϱ�
  # print(n_indep)
  write.csv(n_indep, "output/result.csv")
  
  
  # ȿ��ũ���� ���� 3�� �̻��� �͸�
  indep_vars = names(n_indep[n_indep>=3])
  
  # ����� ���� Ȯ���ϱ� 
  # print(indep_vars)
  write.csv(indep_vars, "output/over3.csv")
  
}

work_meta_analysis <- function(meta_data) {
  ## ��Ÿ�м� ��Ű�� �ҷ�����
  library(metafor)
  
  # �������� ����
  partial_data = subset(meta_data, x=="���ΰ��� �ڱ�ȿ�ɰ�")
  #print(partial_data)
  
  # ��Ÿ�м�
  meta_obj = rma(ri=Correlation, ni=N, data=partial_data, measure="ZCOR")
  #print(meta_obj)
  
  # ��ջ�����
  meta_cor = predict(meta_obj, transf=transf.ztor)
  print("��ջ�����")
  print(meta_cor)
  
  ## ��Ÿ�м� ȿ��ũ�� �׸�
  png(filename="output/meta1.png")
  forest(meta_obj, transf=transf.ztor)
  dev.off()
  print("export : ��Ÿ�м� ȿ��ũ�� �׸�")
  
  ## ������
  study_name = paste0(partial_data$����, " (", partial_data$���ǳ⵵, ")")
  
  ## ������ �ְ� �ٽ� �׸���
  png(filename="output/meta2.png")
  forest(meta_obj, transf=transf.ztor, slab=study_name)
  dev.off()
  print("export : ��Ÿ�м� ȿ��ũ�� �׸� (������ ����)")
  
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