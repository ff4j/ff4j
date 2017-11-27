package org.ff4j.couchbase.store;

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
