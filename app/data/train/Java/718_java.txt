package com.nilsonmassarenti.app.currencyfair.model;

/**
 * This class is to manager URL of Rest.
 * @author nilsonmassarenti - nilsonmassarenti@gmail.com
 * @version 0.1
 * Last update: 03-Mar-2015 12:20 am
 */
public class CurrencyFairURI {
	public static final String DUMMY_BP = "/rest/currencyfair/dummy";
	public static final String GET_CURRENCY_FAIR = "/rest/currencyfair/get/{id}";
	public static final String GET_ALL_CURRENCY_FAIR = "/rest/currencyfairs";
	public static final String CREATE_CURRENCY_FAIR = "/rest/currencyfair/create";
	public static final String DELETE_CURRENCY_FAIR = "/rest/currencyfair/delete/{id}";
}
