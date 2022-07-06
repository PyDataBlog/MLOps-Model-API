package com.lms.dao;

import java.util.List;

import com.lms.jpa.Person;


public interface DAOPerson {

	public boolean personExist(long personId);

    public void insertPerson(Person person);

    public void updatePerson(Person person);

    public void deletePerson(long personId);

    public Person fetchPersonInfo(long personId);

    public List<Person> fetchAllPerson();
    
}
