## Queries Neo4j

### Creation    

* Create test case
```
CREATE (AwesomeFeature:FF4J_FEATURE { uid:'AwesomeFeature', enable:true, description:'some desc' }),
       (first:FF4J_FEATURE { uid:'first',  enable:true, description:'first',  roles:['USER'] }),
 	   (ppint:FF4J_PROPERTY { name:'ppint', type:'org.ff4j.property.PropertyInt', value:'12' }),
	   (ppdouble:FF4J_PROPERTY { name:'ppdouble', value:'12.5' }),
	   (ppboolean:FF4J_PROPERTY { name:'ppboolean', value:'true' }),
	   (ppstring:FF4J_PROPERTY { name:'ppstring', value:'hello' }),
	   (ppListInt:FF4J_PROPERTY { name:'ppListInt', value:'12,13,14' }),
	   (myLogLevel:FF4J_PROPERTY { name:'myLogLevel', value:'DEBUG', type:'org.ff4j.property.PropertyLogLevel' }),
	   (digitValue:FF4J_PROPERTY { name:'digitValue', value:'1', type:'org.ff4j.property.PropertyInt', fixedValues: ['0','1','2','3'] }),
	   (regionIdentifier:FF4J_PROPERTY { name:'regionIdentifier', value:'AMER', fixedValues: ['AMER','SSSS','EAST','EAST'] }),
	   ppdouble-[:PROPERTY_OF]->first,
	   ppboolean-[:PROPERTY_OF]->first,
	   ppstring-[:PROPERTY_OF]->first,
	   ppListInt-[:PROPERTY_OF]->first,
	   myLogLevel-[:PROPERTY_OF]->first,
	   digitValue-[:PROPERTY_OF]->first,
	   regionIdentifier-[:PROPERTY_OF]->first, 
	   (GRP0:FF4J_FEATURE_GROUP { name:'GRP0' }),
	   (second:FF4J_FEATURE { uid:'second', enable:false, description:'second', roles:['USER'] }),
	   (second)-[:MEMBER_OF]->(GRP0),   
	   (GRP1:FF4J_FEATURE_GROUP { name:'GRP1' }),
	   (third:FF4J_FEATURE { uid:'third', enable:false, description:'third', roles:['ADMINISTRATOR', 'BETA-TESTER'] }),
	   (third)-[:MEMBER_OF]->(GRP1),  
	   (forth:FF4J_FEATURE { uid:'forth', enable:true, description:'forth', roles:['ADMINISTRATOR', 'BETA-TESTER'] }),
	   (stratforth:FF4J_FLIPPING_STRATEGY { type:'org.ff4j.strategy.el.ExpressionFlipStrategy', initParams: [ 'expression=third|second' ]}),
	   (stratforth)-[:STRATEGY_OF]->forth,
	   (stratforth)-[:STRATEGY_OF]->first,
	   (forth)-[:MEMBER_OF]->(GRP1), 
	   (a:FF4J_PROPERTY { name:'a', value:'AMER',fixedValues: ['AMER','EAST','EAST','EAST'] }),
	   (b:FF4J_PROPERTY { name:'b', value:'12' }),
	   (c:FF4J_PROPERTY { name:'c', value:'12.5' }),
	   (d:FF4J_PROPERTY { name:'d', value:'true' }),
	   (e:FF4J_PROPERTY { name:'e', value:'hello' }),
	   (f:FF4J_PROPERTY { name:'f', value:'12,13,14' }),
	   (g:FF4J_PROPERTY { name:'g', value:'DEBUG', type:'org.ff4j.property.PropertyLogLevel'  });
```

### READ

* Test if a feature exist
```
MATCH (f:FF4J_FEATURE { uid:  {uid} } ) RETURN count(*) AS NB;
```

* Test if a group exist
```
MATCH (f:FF4J_FEATURE_GROUP { name:  {groupName} } ) RETURN count(*) AS NB;
```

* Read a feature will all relationships
```
MATCH (f:FF4J_FEATURE { uid: {uid} })--(all) RETURN f,all;
```

* Read a feature will no relationships
```
MATCH (f:FF4J_FEATURE { uid: {uid} }) RETURN f;
```

* Read all features and relations
```
MATCH (f:FF4J_FEATURE)--(all) RETURN f,all;
```

* Read all features wihtout relations
```
MATCH (f:FF4J_FEATURE) RETURN f;
```



    /** Cypher query. */
    String QUERY_CYPHER_GETGROUPNAME = 
            "MATCH(f:" + FF4jNeo4jLabels.FF4J_FEATURE + "  { uid:  {uid} } ) " + 
             "--(g:" + FF4jNeo4jLabels.FF4J_FEATURE_GROUP + ") " + 
             "RETURN g.name as GROUPNAME;";
    
    /** Cypher query. */
    String QUERY_CYPHER_GET_FLIPPINGSTRATEGY = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} })" + 
            "--(s:" + FF4jNeo4jLabels.FF4J_FLIPPING_STRATEGY + ") " + 
            "RETURN s;";
    
    /** Cypher query. */
    String QUERY_CYPHER_COUNT_FEATURE_OF_GROUP = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " ) " + 
            "WHERE (f)-[:MEMBER_OF]-( { name: {groupName} }) " + 
            "RETURN COUNT(*) AS " + QUERY_CYPHER_ALIAS + ";";
    
    /** Cypher query. */
    String QUERY_CYPHER_READ_FEATURES_OF_GROUP = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " ) " + 
            "WHERE (f)-[:MEMBER_OF]-( { name: {groupName} }) " + 
            "RETURN f.uid AS UID;";
    
    /** Cypher query. */
    String QUERY_READ_GROUPS = 
            "MATCH (g:" +  FF4jNeo4jLabels.FF4J_FEATURE_GROUP + "  ) " + 
            "RETURN g.name AS GROUPNAME;";
            
            
### UPDATE

* Add a feature to a group
```
MATCH (f:FF4J_FEATURE { uid: 'first' } ), (g:FF4J_FEATURE_GROUP {name: 'GRP0'}) " + 
CREATE (f)-[:MEMBER_OF]->(g);
```


         
### DELETE

    
    // -------------------------------------------------------
    // --------------------- Update  -------------------------  
    // -------------------------------------------------------
    
    /** Cypher query. */
    String QUERY_CYPHER_ENABLE  = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} }) " + 
            "SET f.enable = true RETURN f.enable;";
    
    /** Cypher query. */
    String QUERY_CYPHER_DISABLE = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} }) " + 
            "SET f.enable = false RETURN f.enable;";
    
    /** Cypher query. */
    String QUERY_CYPHER_ADD_ROLE = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + "  {uid: {uid} }) " + 
            "SET f.roles = f.roles + {roleName} return f;";
    
    /** Cypher query. */
    String QUERY_CYPHER_ENABLE_GROUP = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " ) " + 
            "WHERE (f)-[:MEMBER_OF]-( { name: {groupName} }) " + 
            "SET f.enable = true RETURN f.enable;";
    
    /** Cypher query. */
    String QUERY_CYPHER_DISABLE_GROUP = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " ) " + 
            "WHERE (f)-[:MEMBER_OF]-( { name: {groupName} }) " + 
            "SET f.enable = false RETURN f.enable;";
    
  
    
    /** Cypher query. */
    String QUERY_CYPHER_UPDATE_ROLE = 
            "MATCH (f:FF4J_FEATURE { uid: {uid}  }) " + 
            "SET f.roles = {roles} RETURN f";
    
    // -------------------------------------------------------
    // --------------------- Delete --------------------------
    // -------------------------------------------------------
    
    /** Delete properties related to the feature. */
    String QUERY_CYPHER_DELETE_PROPERTIES_FEATURE = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} })--(p:" + FF4jNeo4jLabels.FF4J_PROPERTY + " ) "  + 
            "DETACH DELETE p;";
    
    /** Delete flipping strategy related to the feature. */
    String QUERY_CYPHER_DELETE_STRATEGY_FEATURE = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} })--(s:" + FF4jNeo4jLabels.FF4J_FLIPPING_STRATEGY + ") "  + 
            "DETACH DELETE s;";

    /** Delete flipping strategy related to the feature. */
    String QUERY_CYPHER_DELETE_GROUP_FEATURE = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} })--(s:" + FF4jNeo4jLabels.FF4J_FEATURE_GROUP + ") "  + 
            "DETACH DELETE s;";
    
    /** Delete Feature with all its relationships*/
    String QUERY_CYPHER_DELETE_FEATURE = 
            "MATCH (f:FF4J_FEATURE { uid: {uid}  }) " + 
            "DETACH DELETE f;";
    
    /** Cypher query. */
    String QUERY_CYPHER_REMOVEFROMGROUP = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} })-[a:MEMBER_OF]->() DELETE a;";
    
    /** Cypher query. */
    String QUERY_CYPHER_DELETE_GROUP = 
            "MATCH (g:" + FF4jNeo4jLabels.FF4J_FEATURE_GROUP + " { name: {groupName} }) DETACH DELETE g;";
   
   