import { expect } from 'chai'
import {List, Map} from 'immutable'
import categories from '../src/reducer.js'

describe("Category Test", () => {

	it("should add a category", () => {
		let initialState = Map({
			user: 'Skye'
		})
		let expectedState = Map({
			user: 'Skye',
			categories: Map({
				'Love': 0
			})
		})
		let result = categories(initialState, {type:'ADD_CATEGORY', value:'Love'})
		expect(result).to.eql(expectedState)
	})

	it("adding a category does not modify previous state", () => {
		let initialState = Map({
			user: 'Skye',
			categories: Map({
				'Love': 90
			})
		})
		let expectedState = Map({
			user: 'Skye',
			categories: Map({
				'Love': 90,
				'Communication': 0
			})
		})
		let result = categories(initialState, {type:'ADD_CATEGORY', value:'Communication'})
		expect(result).to.eql(expectedState)
		expect(initialState).to.eql(Map({
			user: 'Skye',
			categories: Map({
				'Love': 90
			})
		}))
	})

	it("should rate a category", () => {
		let initialState = Map({
			user: 'Skye',
			categories: Map({
				'Love': 0
			})
		})
		let expectedState = Map({
			user: 'Skye',
			categories: Map({
				'Love': 90
			})
		})
		let result = categories(initialState, { type:'RATE_CATEGORY', name:'Love', value:90 })
		expect(result).to.eql(Map({
			user: 'Skye',
			categories: Map({
				'Love': 90
			})
		}))
	})

	it("should stop adding categories", () => {
		let initialState = Map({
			user: 'Skye',
			categories: Map({
				'Love': 90,
				'Communication': 80,
				'Fun' : 100
			})
		})
	  let expectedState = Map({
			user: 'Skye',
			categories: Map({
				'Love': 90,
				'Communication': 80,
				'Fun' : 100
			}),
			categories_finished: true
		})
		let result = categories(initialState, {type:'FINISH_CATEGORIES', value: true})
		expect(result).to.eql(expectedState)
	})

})	


