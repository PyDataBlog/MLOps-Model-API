import { Increment, Decrement } from '../actions'

export default (state = 0, action) => {
  switch (action.constructor) {
    case Increment:
      return state + 1
    case Decrement:
      return state - 1
    default:
      return state
  }
}
