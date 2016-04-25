package org.ff4j.audit;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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


import java.io.Serializable;
import java.util.Set;

/**
 * Sample Query into Event Repository.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class EventQueryDefinition implements Serializable {
	
	/** Serial. */
	private static final long serialVersionUID = -1649081647715140190L;

	/** bound bottom. */
	private Long from;
	
	/** bound top. */
	private Long to;
	
	/** filter about target. */
	private Set < String > targetsFilter = null;
	
	/** filter about names. */
	private Set < String > namesFilter = null;
	
	/** filter about action. */
	private Set < String > actionFilter = null;
	
	/** Sort at end the end if required. */
	private EventQueryCriteria sortCriteria;
	
	/**
	 * Default constucot
	 */
	public EventQueryDefinition() {
	}
	
	/**
	 * Default constucot
	 */
	public EventQueryDefinition(long from, long to) {
		this.from = from;
		this.to = to;
	}

	/**
	 * Getter accessor for attribute 'from'.
	 *
	 * @return
	 *       current value of 'from'
	 */
	public Long getFrom() {
		return from;
	}

	/**
	 * Setter accessor for attribute 'from'.
	 *
	 * @param from
	 * 		new value for 'from '
	 */
	public void setFrom(Long from) {
		this.from = from;
	}

	/**
	 * Getter accessor for attribute 'to'.
	 *
	 * @return
	 *       current value of 'to'
	 */
	public Long getTo() {
		return to;
	}

	/**
	 * Setter accessor for attribute 'to'.
	 *
	 * @param to
	 * 		new value for 'to '
	 */
	public void setTo(Long to) {
		this.to = to;
	}

	/**
	 * Getter accessor for attribute 'targetsFilter'.
	 *
	 * @return
	 *       current value of 'targetsFilter'
	 */
	public Set<String> getTargetsFilter() {
		return targetsFilter;
	}

	/**
	 * Setter accessor for attribute 'targetsFilter'.
	 *
	 * @param targetsFilter
	 * 		new value for 'targetsFilter '
	 */
	public void setTargetsFilter(Set<String> targetsFilter) {
		this.targetsFilter = targetsFilter;
	}

	/**
	 * Getter accessor for attribute 'namesFilter'.
	 *
	 * @return
	 *       current value of 'namesFilter'
	 */
	public Set<String> getNamesFilter() {
		return namesFilter;
	}

	/**
	 * Setter accessor for attribute 'namesFilter'.
	 * @param namesFilter
	 * 		new value for 'namesFilter '
	 */
	public void setNamesFilter(Set<String> namesFilter) {
		this.namesFilter = namesFilter;
	}

	/**
	 * Getter accessor for attribute 'actionFilter'.
	 *
	 * @return
	 *       current value of 'actionFilter'
	 */
	public Set<String> getActionFilter() {
		return actionFilter;
	}

	/**
	 * Setter accessor for attribute 'actionFilter'.
	 *
	 * @param actionFilter
	 * 		new value for 'actionFilter '
	 */
	public void setActionFilter(Set<String> actionFilter) {
		this.actionFilter = actionFilter;
	}

	/**
	 * Getter accessor for attribute 'sortCriteria'.
	 *
	 * @return
	 *       current value of 'sortCriteria'
	 */
	public EventQueryCriteria getSortCriteria() {
		return sortCriteria;
	}

	/**
	 * Setter accessor for attribute 'sortCriteria'.
	 * @param sortCriteria
	 * 		new value for 'sortCriteria '
	 */
	public void setSortCriteria(EventQueryCriteria sortCriteria) {
		this.sortCriteria = sortCriteria;
	}
}
