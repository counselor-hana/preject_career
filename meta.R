source('lib.R')

prepare()

meta_data = read_data()

work_indep(meta_data)

work_meta_analysis(meta_data)

print("DONE")
