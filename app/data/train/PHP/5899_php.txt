<?php
namespace Plenty\Modules\Listing\Market\Contracts;

use Illuminate\Database\Eloquent\ModelNotFoundException;
use Plenty\Exceptions\ValidationException;
use Plenty\Modules\Listing\Market\Models\ListingMarket;
use Plenty\Repositories\Contracts\FilterableContract;
use Plenty\Repositories\Criteria\Contracts\CriteriableContract;
use Plenty\Repositories\Criteria\Contracts\ExtendsCriteriaContract;
use Plenty\Repositories\Criteria\Criteria;
use Plenty\Repositories\Models\PaginatedResult;

/**
 * The ListingMarketRepositoryContract is the interface for the listing market repository. This interface allows you to get a single listing market or lists of listing markets as well as to create a listing market.
 */
interface ListingMarketRepositoryContract 
{

	/**
	 * Get a listing market
	 */
	public function get(
		int $id, 
		array $with = []
	):ListingMarket;

	/**
	 * List listing markets
	 */
	public function search(
		int $page = 1, 
		int $itemsPerPage = 50, 
		array $with = [], 
		array $filters = []
	):PaginatedResult;

	/**
	 * Find listing markets
	 */
	public function find(
		int $page = 1, 
		int $itemsPerPage = 50, 
		array $with = [], 
		array $filters = []
	):PaginatedResult;

	/**
	 * Create a listing
	 */
	public function create(
		array $data
	):array;

	/**
	 * Update listing market
	 */
	public function update(
		int $id, 
		array $data
	):ListingMarket;

	/**
	 * Start listing markets on the designated markets.
	 */
	public function start(
		array $ids, 
		array $params = []
	):array;

	/**
	 * Verifies listing markets.
	 */
	public function verify(
		array $ids
	):array;

	/**
	 * Delete a listing market
	 */
	public function delete(
		int $id
	);

	/**
	 * Resets all Criteria filters by creating a new instance of the builder object.
	 */
	public function clearCriteria(
	);

	/**
	 * Applies criteria classes to the current repository.
	 */
	public function applyCriteriaFromFilters(
	);

	/**
	 * Sets the filter array.
	 */
	public function setFilters(
		array $filters = []
	);

	/**
	 * Returns the filter array.
	 */
	public function getFilters(
	);

	/**
	 * Returns a collection of parsed filters as Condition object
	 */
	public function getConditions(
	);

	/**
	 * Clears the filter array.
	 */
	public function clearFilters(
	);

}