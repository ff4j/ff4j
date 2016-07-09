package org.ff4j.mongo.mapper;

import static org.ff4j.mongo.MongoDbConstants.PROPERTY_DESCRIPTION;
import static org.ff4j.mongo.MongoDbConstants.PROPERTY_FIXEDVALUES;
import static org.ff4j.mongo.MongoDbConstants.PROPERTY_NAME;
import static org.ff4j.mongo.MongoDbConstants.PROPERTY_TYPE;
import static org.ff4j.mongo.MongoDbConstants.PROPERTY_VALUE;

import java.util.ArrayList;

import org.bson.Document;
import org.ff4j.mapper.PropertyMapper;
import org.ff4j.property.Property;
import org.ff4j.property.util.PropertyJsonBean;

import com.mongodb.BasicDBList;
import com.mongodb.DBObject;

/**
 * Implementation of {@link PropertyMapper} to work with MongoDb.
 
 * @author Cedrick LUNVEN (@clunven)
 */
public class MongoPropertyMapper implements PropertyMapper<Document> {

    /** {@inheritDoc} */
    @Override
    public Document toStore(Property<?> bean) {
        PropertyJsonBean pjb = new PropertyJsonBean(bean);
        return new PropertyDocumentBuilder().//
                addName(pjb.getName()). //
                addType(pjb.getType()). //
                addValue(pjb.getValue()). //
                addDescription(pjb.getDescription()). //
                addFixedValues(pjb.getFixedValues()).build();
    }

    /** {@inheritDoc} */
    @Override
    @SuppressWarnings("unchecked")
    public Property<?> fromStore(Document dbObject) {
        PropertyJsonBean pf = new PropertyJsonBean();
        pf.setName((String) dbObject.get(PROPERTY_NAME));
        pf.setDescription((String) dbObject.get(PROPERTY_DESCRIPTION));
        pf.setType((String) dbObject.get(PROPERTY_TYPE));
        pf.setValue((String) dbObject.get(PROPERTY_VALUE));
        if (dbObject.containsKey(PROPERTY_FIXEDVALUES)) {
            ArrayList<String> dbList = (ArrayList<String>) dbObject.get(PROPERTY_FIXEDVALUES);
            if (dbList != null) {
                for(Object item : dbList) {
                    pf.addFixedValue((String) item);
                }
            }
        }
        return pf.asProperty();
    }
    
    /**
     * Map a property.
     *
     * @param dbObject
     *      db object
     * @return
     *      list of property
     */
    public Property< ? > fromStore(DBObject dbObject) {
        PropertyJsonBean pf = new PropertyJsonBean();
        pf.setName((String) dbObject.get(PROPERTY_NAME));
        pf.setDescription((String) dbObject.get(PROPERTY_DESCRIPTION));
        pf.setType((String) dbObject.get(PROPERTY_TYPE));
        pf.setValue((String) dbObject.get(PROPERTY_VALUE));
        if (dbObject.containsField(PROPERTY_FIXEDVALUES)) {
            BasicDBList dbList = (BasicDBList) dbObject.get(PROPERTY_FIXEDVALUES);
            if (dbList != null) {
                for(Object item : dbList) {
                    pf.addFixedValue((String) item);
                }
            }
        }
        return pf.asProperty();
    }

}
