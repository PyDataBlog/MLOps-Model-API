package com.treasurehunter.server.persistence.dao;

import com.google.appengine.api.datastore.Key;
import com.treasurehunter.server.persistence.PersistenceManagerFactoryUtil;
import com.treasurehunter.server.persistence.model.DataStoreModel;

import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;
import javax.jdo.Query;
import javax.jdo.Transaction;
import java.util.List;

/**
 * @author Daniel Kocsis
 */
public abstract class BaseDAO<T extends DataStoreModel> implements DAO<T> {

	public T create(T model) {
		PersistenceManager persistenceManager = getPersistenceManager();

		try {
			return persistenceManager.makePersistent(model);
		}
		finally {
			persistenceManager.close();
		}
	}

	public void delete(T model) {
		PersistenceManager persistenceManager = getPersistenceManager();

		try {
			persistenceManager.deletePersistent(model);
		}
		finally {
			persistenceManager.close();
		}
	}

	public List<T> findAll(Class<T> clazz) {
		PersistenceManager persistenceManager = getPersistenceManager();
		List entries;

		Query query = persistenceManager.newQuery(clazz);
		query.setFilter("flgNull == \"N\"");
		query.setOrdering("id asc");

		try {
			entries = (List) query.execute();

			return entries;
		}
		finally {
			persistenceManager.close();
		}
	}

	@Override
	public T update(T model) {
		PersistenceManager persistenceManager = getPersistenceManager();
		Transaction transaction = persistenceManager.currentTransaction();

		try {
			transaction.begin();

			model = persistenceManager.makePersistent(model);

			transaction.commit();
		}
		finally {
			if (transaction.isActive()) {
				transaction.rollback();
			}
		}

		return model;
	}

	protected T getModelByKey(Class<T> clazz, Key key) {
		PersistenceManager persistenceManager = getPersistenceManager();

		try {
			T existingModel = persistenceManager.getObjectById(clazz, key);

			return persistenceManager.detachCopy(existingModel);
		}
		finally {
			persistenceManager.close();
		}
	}

	protected PersistenceManager getPersistenceManager() {
		PersistenceManagerFactory pmf =
			PersistenceManagerFactoryUtil.getPersistenceManagerFactory();

		return pmf.getPersistenceManager();
	}

}
