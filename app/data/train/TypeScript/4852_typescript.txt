class ListController implements ng.IComponentController {
    private itemsList: Array<any>;
    private sortType: string;
    private sortReverse: Boolean;

    constructor($stateParams) {
        console.log('list controller');
        console.log("this.itemsList=", this.itemsList);
    }

    private sort(fieldName) {
        console.log('sort called!');
        if (this.sortType !== fieldName) {
            this.sortType = fieldName;
            this.sortReverse = true;
        } else {
            this.sortReverse = !this.sortReverse;
        }
    }
}

export default ListController;
ListController.$inject = ['$stateParams'];
