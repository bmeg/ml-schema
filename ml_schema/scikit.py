

from ml_schema import ml_schema_pb2

def proto_randomforest(clf):
    forest = ml_schema_pb2.RandomForestData()
    for e in clf.estimators_:
        tree = ml_schema_pb2.DecisionTree()
        for i in xrange(e.tree_.node_count):
            node = ml_schema_pb2.DecisionTree.DecisionNode(
                AboveChild=e.tree_.children_left[i],
                BelowChild=e.tree_.children_right[i],
                SplitVariable=str(e.tree_.feature[i]),
                SplitValue=e.tree_.threshold[i],
                LeafNode=False,
                Label=False
            )
            tree.Nodes.extend([node])
        forest.Forest.extend([tree])
    return forest

def proto_linear(clf, feature_names=None):
    linear = ml_schema_pb2.LinearCoeffData()
    linear.Intercept = clf.intercept_
    for i, coeff in enumerate(clf.coef_):
        name = str(i)
        if feature_names is not None:
            name = feature_names[i]
        coeff = ml_schema_pb2.FeatureCoefficient(
            Feature=name,
            Coeff=coeff
        )
        linear.Coeff.extend([coeff])
    model = ml_schema_pb2.ModelStructure(
        Components=[
            ml_schema_pb2.ModelComponent(Coeff=1,LinearCoeff=linear)
        ]
    )
    return model
    