.PHONY: clean

clean:
	rm -rf Figures
	mkdir -p Figures
	
Figures/SE_smoking.jpeg Figures/SleepDuration_BMI.jpeg:\
 Figures/SleepDuration_BMI.jpeg ./Data/Sleep_Efficiency.csv\
 ./Program/01_Data_Exploration.R
	Rscript ./Program/01_Data_Exploration.R
