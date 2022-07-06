package com.guitar.db;

import static org.junit.Assert.*;

import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

import com.guitar.db.model.Location;
import com.guitar.db.repository.LocationJpaRepository;
import com.guitar.db.repository.LocationRepository;

@ContextConfiguration(locations={"classpath:com/guitar/db/applicationTests-context.xml"})
@RunWith(SpringJUnit4ClassRunner.class)
public class LocationPersistenceTests {
//	@Autowired
//	private LocationRepository locationRepository;
	
	@Autowired
	private LocationJpaRepository locationJpaRepository;

	@PersistenceContext
	private EntityManager entityManager;

	@Test
	public void testJpaFind(){
		List<Location> locations = locationJpaRepository.findAll();
		assertNotNull(locations);
	}
	
	@Test
	public void testJpaNot(){
		List<Location> locations = locationJpaRepository.findByStateNot("Alabama");
		assertNotSame("Alabama", locations.get(0).getState());
	}
	
	@Test
	public void testJpaAnd(){
		List<Location> locations = locationJpaRepository.findByStateAndCountry("Utah", "United States");
		assertNotNull(locations);
		
		assertEquals("Utah", locations.get(0).getState());
	}
	
	@Test
	public void testJpaOr(){
		List<Location> locations = locationJpaRepository.findByStateIsOrCountryEquals("Utah", "France");
		assertNotNull(locations);
		
		assertEquals("Utah", locations.get(0).getState());
	}
	
	@Test
	@Transactional
	public void testSaveAndGetAndDelete() throws Exception {
		Location location = new Location();
		location.setCountry("Canada");
		location.setState("British Columbia");
		//location = locationRepository.create(location);
		location = locationJpaRepository.saveAndFlush(location);
		
		// clear the persistence context so we don't return the previously cached location object
		// this is a test only thing and normally doesn't need to be done in prod code
		entityManager.clear();

		//Location otherLocation = locationRepository.find(location.getId());
		Location otherLocation = locationJpaRepository.findOne(location.getId());
		assertEquals("Canada", otherLocation.getCountry());
		assertEquals("British Columbia", otherLocation.getState());
		
		//delete BC location now
		//locationRepository.delete(otherLocation);
		locationJpaRepository.delete(otherLocation);
	}

	@Test
	public void testFindWithLike() throws Exception {
		List<Location> locs = locationJpaRepository.findByStateLike("New%");//locationRepository.getLocationByStateName("New");
		assertEquals(4, locs.size());
	}
	
	@Test
	public void testFindWithLikeIgnoreCase() throws Exception {
		List<Location> locs = locationJpaRepository.findByStateLikeIgnoreCase("new%");//locationRepository.getLocationByStateName("New");
		assertEquals(4, locs.size());
	}
	
	@Test
	public void testFindFirstWithLikeIgnoreCase() throws Exception {
		Location loc = locationJpaRepository.findFirstByStateLikeIgnoreCase("new%");//locationRepository.getLocationByStateName("New");
		assertNotNull(loc);
	}
	
	@Test
	public void testFindStartingWith() throws Exception {
		List<Location> locs = locationJpaRepository.findByStateStartingWith("New");//locationRepository.getLocationByStateName("New");
		assertEquals(4, locs.size());
	}
	
	@Test
	public void testFindWithNotLike() throws Exception {
		List<Location> locs = locationJpaRepository.findByStateNotLike("New%");//locationRepository.getLocationByStateName("New");
		assertEquals(46, locs.size());
	}
	
	@Test
	public void testFindWithNotLikeOrderBy() throws Exception {
		List<Location> locs = locationJpaRepository.findByStateNotLikeOrderByStateAsc("New%");//locationRepository.getLocationByStateName("New");
		assertEquals("Alabama", locs.get(0).getState());
		assertEquals(46, locs.size());
		
		for(Location loc : locs){
			System.out.println(loc.getState());
		}
	}

	@Test
	@Transactional  //note this is needed because we will get a lazy load exception unless we are in a tx
	public void testFindWithChildren() throws Exception {
		Location arizona = locationJpaRepository.findOne(3L);//locationRepository.find(3L);
		assertEquals("United States", arizona.getCountry());
		assertEquals("Arizona", arizona.getState());
		
		assertEquals(1, arizona.getManufacturers().size());
		
		assertEquals("Fender Musical Instruments Corporation", arizona.getManufacturers().get(0).getName());
	}
}
