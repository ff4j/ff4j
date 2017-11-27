package org.ff4j.couchbase.store;

/*
 * #%L
 * ff4j-store-couchbase
 * %%
 * Copyright (C) 2013 - 2017 FF4J
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */

import java.util.List;

import com.couchbase.client.java.Bucket;
import com.couchbase.client.java.view.ViewQuery;
import com.couchbase.client.java.view.ViewResult;
import com.couchbase.client.java.view.ViewRow;

/**
 * Query on View and not more in Bucket as advanced queries.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class EventRepositoryCouchbase {

    public void queryView(Bucket bucket, String design, String view) {
        ViewQuery queryAllFeatures = ViewQuery.from(design, view);
        ViewResult queryResult = bucket.query(queryAllFeatures);
        List<ViewRow> rows = queryResult.allRows();
        for (ViewRow viewRow : rows) {
            System.out.println("Result view:" + viewRow.id() + ":" + viewRow.value());
        }
    }

}
