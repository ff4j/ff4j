package org.ff4j;

/**
 * Public Interface of a Listener on CRUD repository.
 * @Do not put any onRead() as making not sense in ff4J.
 * 
 * @author Cedrick LUNVEN  (@clunven)
 *
 * @param <ENTITY>
 *    {@link FF4jEntity} to be specialized by type of store 
 */
public interface FF4jRepositoryListener < ENTITY extends FF4jEntity<?> > {
    
    /**
     * Invoked when an entity is created into the repository.
     * 
     * @param ENTITY
     *      object manipulated by the framework
     */
    void onCreate(ENTITY bo);
    
    /**
     * Invoked when an entity is deleted from the repository.
     * 
     * @param ENTITY
     *      object manipulated by the framework
     */
    void onDelete(String uid);
    
    /**
     * Invoked when deleting all entities of a repository.
     */
    void onDeleteAll();
    
    /**
     * Invoked when an entity is updated within repository.
     * 
     * @param ENTITY
     *      object manipulated by the framework
     */
    void onUpdate(ENTITY bo);
    
    /**
     * Invoked when the schema creation method is invoked.
     * 
     * @param ENTITY
     *      object manipulated by the framework
     */
    void onCreateSchema();
    
}
