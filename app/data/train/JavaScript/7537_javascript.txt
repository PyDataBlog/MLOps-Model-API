import { createStore } from 'redux'
import { combineReducers } from 'redux';
import { todoReducer } from '../todo-reducer';
import { appReducer } from '../app-reducer';

const todoApp = combineReducers({
  todos: todoReducer,
  app: appReducer
})

export const store = createStore(todoApp);

store.getState()

// outputs ->
// {
//   todos: {
//     todos: []
//   },
//   app: {
//     settingsVisible: false,
//     ...
//   }
// }
