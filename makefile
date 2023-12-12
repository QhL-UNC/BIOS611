.PHONY: clean

clean:
	rm -rf Figures
	mkdir -p Figures

all: Figures/SE_vs_Alcohol.jpeg Figures/SE_vs_Awakenings.jpeg\
	Figures/SE_Correlation.jpeg Figures/Age_vs_Caffeine.jpeg\
	Figures/TST_Correlation.jpeg\
	Figures/SleepDisorder_vs_SBP_TST.jpeg\
	Figures/SleepDisorder_vs_Stress_TST.jpeg

Figures/SE_vs_Alcohol.jpeg Figures/SE_vs_Awakenings.jpeg Figures/SE_Correlation.jpeg:\
	./Data/Sleep_Efficiency.csv ./Program/03_MoreExploration.R
	Rscript ./Program/03_MoreExploration.R

Figures/Age_vs_Caffeine.jpeg:\
	./Data/Sleep_Efficiency.csv ./Program/04_Linear_Model.R
	Rscript ./Program/04_Linear_Model.R

Figures/TST_Correlation.jpeg: \
	./Data/Sleep_health_and_lifestyle_dataset.csv ./Program/03_MoreExploration.R
	Rscript ./Program/03_MoreExploration.R

Figures/SleepDisorder_vs_SBP_TST.jpeg Figures/SleepDisorder_vs_Stress_TST.jpeg:\
	./Data/Sleep_health_and_lifestyle_dataset.csv ./Program/04_Linear_Model.R
	Rscript ./Program/04_Linear_Model.R

	