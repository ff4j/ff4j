package org.ff4j.store;

/*
 * #%L ff4j-store-jdbc %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import com.mongodb.BasicDBList;
import com.mongodb.BasicDBObject;
import com.mongodb.BasicDBObjectBuilder;
import com.mongodb.DBCollection;
import com.mongodb.DBObject;
import org.bson.types.BasicBSONList;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlipStrategy;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.utils.ParameterUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of {@link FeatureStore} to work with MongoDB.
 *
 *
 * Nota : _id == uuid
 */
public class FeatureStoreMongoDB implements JdbcFeatureStoreConstants, FeatureStore {

    private enum Field {
        UUID("_id"), //
        ENABLE("enable"), //
        DESCRIPTION("description"), //
        STRATEGY("strategy"), //
        EXPRESSION("expression"), //
        GROUPNAME("groupname"), //
        ROLES("roles");

        private final String column;

        private Field(String column) { this.column = column; }

        @Override
        public String toString() { return column; };
    }

    // Mutable object.
    private static class Builder {
        private final BasicDBObjectBuilder build = new BasicDBObjectBuilder();

        public static Builder create(Field field, Object value) { return new Builder().add(field, value); }

        public static Builder createUUID(String value) { return new Builder().add(Field.UUID, value); }

        public Builder add(Field field, Object value) {
            build.add(field.column, value);
            return this;
        }

        public DBObject get() { return build.get(); }
    }

    /** Row Mapper for FlipPoint. */
    private static final FlippingPointRowMapper MAPPER = new FlippingPointRowMapper();

    /** MongoDB collection. */
    private final DBCollection collection;

    /**
     * Mapper from Database to FlippingPoint.
     */
    private static class FlippingPointRowMapper {
        public FlippingPointRowMapper() {};

        public Feature mapRow(DBObject dbObject) {
            String featUid = (String) dbObject.get(Field.UUID.toString());
            return new Feature(featUid,
                    (Boolean) dbObject.get(Field.ENABLE.toString()),
                    (String) dbObject.get(Field.DESCRIPTION.toString()),
                    (String) dbObject.get(Field.GROUPNAME.toString()),
                    mapAuthorization(dbObject),
                    mapStrategy(featUid, dbObject));
        }

        private Collection<String> mapAuthorization(DBObject dbObject) {
            List<String> authorisation = new ArrayList<String>();
            if (dbObject.containsField(Field.ROLES.toString())) {
                for (Object role : (BasicBSONList) dbObject.get(Field.ROLES.toString())) {
                    authorisation.add(role.toString());
                }
            }
            return authorisation;
        }

        private FlipStrategy mapStrategy(String featUid, DBObject dbObject) {
            String strategy = (String) dbObject.get(Field.STRATEGY.toString());
            if (strategy != null && !"".equals(strategy)) {
                try {
                    FlipStrategy flipStrategy = (FlipStrategy) Class.forName(strategy).newInstance();
                    flipStrategy.init(featUid, ParameterUtils.toMap((String) dbObject.get(Field.EXPRESSION.toString())));
                    return flipStrategy;
                } catch (InstantiationException ie) {
                    throw new FeatureAccessException("Cannot instantiate Strategy, no default constructor available", ie);
                } catch (IllegalAccessException iae) {
                    throw new FeatureAccessException("Cannot instantiate Strategy, no visible constructor", iae);
                } catch (ClassNotFoundException e) {
                    throw new FeatureAccessException("Cannot instantiate Strategy, classNotFound", e);
                }
            }
            return (FlipStrategy) dbObject.get(Field.STRATEGY.toString());
        }

        public DBObject toDBObject(Feature feature) {
            String strategyColumn = null;
            String expressionColumn = null;
            if (feature.getFlippingStrategy() != null) {
                strategyColumn = feature.getFlippingStrategy().getClass().getCanonicalName();
                expressionColumn = ParameterUtils.fromMap(feature.getFlippingStrategy().getInitParams());
            }
            BasicDBList authorizations = new BasicDBList();
            authorizations.addAll(feature.getAuthorizations());
            return Builder.createUUID(feature.getUid()).add(Field.ENABLE, feature.isEnable())
                    .add(Field.DESCRIPTION, feature.getDescription())
                    .add(Field.GROUPNAME, feature.getGroup())
                    .add(Field.STRATEGY, strategyColumn)
                    .add(Field.EXPRESSION, expressionColumn)
                    .add(Field.ROLES, authorizations)
                    .get();
        }
    }

    /**
     * @param collection
     *            the collection to set
     */
    public FeatureStoreMongoDB(DBCollection collection) { this.collection = collection; }

    /** {@inheritDoc} */
    @Override
    public void enable(String featId) {
        if (!exist(featId)) {
            throw new FeatureNotFoundException(featId);
        }
        collection.update(Builder.create(Field.UUID, featId).get(), BasicDBObjectBuilder.start("$set", Builder.create(Field.ENABLE, true).get()).get() );
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String featId) {
        if (!exist(featId)) {
            throw new FeatureNotFoundException(featId);
        }
        collection.update(Builder.create(Field.UUID, featId).get(), BasicDBObjectBuilder.start("$set", Builder.create(Field.ENABLE, false).get()).get() );
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String featId) {
        return 1 == collection.count(Builder.create(Field.UUID, featId).get());
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String featId) {
        DBObject object = collection.findOne(Builder.createUUID(featId).get());
        if (object==null) {
            throw new FeatureNotFoundException(featId);
        }
        return MAPPER.mapRow(object);
    }

    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
        if (exist(fp.getUid())) {
            throw new FeatureAlreadyExistException(fp.getUid());
        }
        collection.save(MAPPER.toDBObject(fp));
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String fpId) {
        if (!exist(fpId)) {
            throw new FeatureNotFoundException(fpId);
        }
        Feature fp = read(fpId);
        collection.remove(Builder.createUUID(fp.getUid()).get());
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String fpId, String roleName) {
        if (!exist(fpId)) {
            throw new FeatureNotFoundException(fpId);
        }
        collection.update(Builder.createUUID(fpId).get(), new BasicDBObject("$addToSet", Builder.create(Field.ROLES, roleName).get()));
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String fpId, String roleName) {
        if (!exist(fpId)) {
            throw new FeatureNotFoundException(fpId);
        }
        collection.update(Builder.createUUID(fpId).get(), new BasicDBObject("$pull", Builder.create(Field.ROLES, roleName).get()));
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();
        for(DBObject dbObject : collection.find()) {
            Feature feature = MAPPER.mapRow(dbObject);
            mapFP.put(feature.getUid(), feature);
        }
        return mapFP;
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        Feature fpExist = read(fp.getUid());
        collection.save(MAPPER.toDBObject(fp));

        // enable/disable
        if (fp.isEnable() != fpExist.isEnable()) {
            if (fp.isEnable()) {
                enable(fp.getUid());
            } else {
                disable(fp.getUid());
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return "FeatureStoreMongoDB [collection=" + collection.getFullName() + "]";
    }

    @Override
    public void enableGroup(String groupName) {
        // TODO Auto-generated method stub

    }

    @Override
    public void disableGroup(String groupName) {
        // TODO Auto-generated method stub

    }

    @Override
    public void existGroup(String groupName) {
        // TODO Auto-generated method stub

    }

    @Override
    public Map<String, Feature> readGroup(String groupName) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void addToGroup(String featureId, String groupName) {
        // TODO Auto-generated method stub

    }

    @Override
    public void removeFromGroup(String featureId, String groupName) {
        // TODO Auto-generated method stub

    }

}
