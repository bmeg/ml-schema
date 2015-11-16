### populate_ML_protobuf.R ########################################################################
# document elastic net run as protocol buffer according to provided schema 
###################################################################################################
library(glmnet);
# read in sample feature matrix 
sample.feature.matrix <- read.delim(
	'/Users/user/Documents/Pathway/HallmarkGeneSet/2015-07-20_AllSummaryMetrics.txt',
	sep = '\t',
	header = TRUE
	);
# read in drug response data
drug.response <- read.delim(
	'/Users/user/Documents/DrugResponse/CCLE_response_matrix_binary.tab',
	sep = '\t',
	header = TRUE
	);
# set compound 
compound <- colnames(drug.response)[1]
# keep only cells that have sensitive or non-sensitive response
sample.feature.matrix	<- sample.feature.matrix[rownames(sample.feature.matrix) %in% rownames(drug.response),];
drug.response 			<- drug.response[rownames(drug.response) %in% rownames(sample.feature.matrix),];
sample.feature.matrix 	<- sample.feature.matrix[!is.na(drug.response[,compound]),];
drug.response 			<- drug.response[!is.na(drug.response[,compound]),];
# run elastic net cross validation
elastic.net.results <- cv.glmnet(
	x = as.matrix(sample.feature.matrix),
	y = drug.response[,compound], 
	family = 'binomial', 
	alpha = 0.2,
	type.measure = "mse"
	);
###################################################################################################
populate_ML_protobuf <- function(model.object, x.test, y.test, file) {
	library(RProtoBuf)
	# read in protocol buffer schema
	readProtoFiles('/Users/user/ml-schema/proto/ml_schema.proto');

	# extract coefficients
	coefficients <- coef(model.object);
	# only keep features with non-zero coefficients
	collapse.coefficients <- coefficients[coefficients[,1] != 0,];
	# subset out intercept
	intercept 				<- collapse.coefficients[grep('Intercept', names(collapse.coefficients))];
	collapse.coefficients 	<- collapse.coefficients[-grep('Intercept', names(collapse.coefficients))];
	# populate ModelStructure message
	ModelStructure <- new(
		ml_schema.ModelStructure,
		## embedded ModelComponent message ##
		Components = new(
			ml_schema.ModelComponent,
			# additional coefficient argument that am unsure what is for
		## embedded LinearCoeffData message ##
			LinearCoeff = new(
				ml_schema.LinearCoeffData,
				Intercept = intercept,
		## embedded FeatureCoefficient message ##
				Coeff = sapply(
					seq_along(collapse.coefficients),
					function(i) {
						new(
							ml_schema.FeatureCoefficient,
							Feature = names(collapse.coefficients)[i],
							Coeff = collapse.coefficients[i]
							)}))))

	# generate performance stats
	# generate predictions
	predictions <- predict(model.object, as.matrix(x.test), type = 'class');
	# calculate TP/FP/TN/FN
	TP <- sum(y.test == 'sensitive' & predictions == 'sensitive');
	FP <- sum(y.test == 'sensitive' & predictions == 'non-sensitive');
	TN <- sum(y.test == 'non-sensitive' & predictions == 'non-sensitive')
	FN <- sum(y.test == 'non-sensitive' & predictions == 'sensitive');
	# calculate sensitivity and specificity
	sensitivity <- TP/(TP+FN);
	specificity <- TN/(TN+FP);
	# generate ModelPerformance message
	ModelPerformance <- new(
		ml_schema.ModelPerformance,
		ModelID = 'TestingData', 
		## embedded MetricSummary message ##
		Metrics = list(
			new(
				ml_schema.MetricSummary,
				Type = 'Sensitivity',
				Value = sensitivity
				),
			new(
				ml_schema.MetricSummary,
				Type = 'Specificity',
				Value = specificity
				),
			new(
				ml_schema.MetricSummary,
				Type = 'Balanced Accuracy',
				Value = (sensitivity+specificity)/2
				)
			),
		## embedded CutoffMetrics message ##
		Cutoffs = new(
			ml_schema.CutoffMetrics,
		## embedded ConfusionMatrix message ##
			Truth = new(
				ml_schema.ConfusionMatrix,
				TruePositive = TP,
				TrueNegative = TN,
				FalsePositive = FP,
				FalseNegative = FN
				)
			)
		);

	# generate final Model message
	Model <- new(
		ml_schema.Model,
		ID = "Hallmark Gene Set ElasticNet",
		Structure = ModelStructure,
		Performance = ModelPerformance
		)
	# write to file
	serialize(Model, file)
	}