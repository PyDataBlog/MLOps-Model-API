import Vuex from 'vuex'
import store from './file-store'

export default new Vuex.Store({
    state: {
        characters: []
    },
    getters: {
        characters: state => {
            return state.characters;
        },
        fileNames: state => {
            return store.getLists();
        }
    },
    mutations: {
        loadFile(state, fileName) {
            state.characters = JSON.parse(store.loadFile(fileName));
        }
    },
    actions: {
        loadFile(context, fileName) {
            context.commit('loadFile', fileName);
        }
    }
})