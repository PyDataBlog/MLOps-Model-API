import {IDetailsService} from '../services/details.service';

class DetailsController implements ng.IComponentController {
    private detailsService: IDetailsService;
    private detailsData: any;
    private previousState: any;

    constructor($stateParams: any, detailsService: IDetailsService) {
        console.log('details controller');
        console.log("$stateParams.id=", $stateParams.id);
        console.log("this.previousState=", this.previousState);
        let id = $stateParams.id;
        detailsService.searchByIMDbID(id, 'full').then((result) => {
            console.log("detailsData = ", result.data);
            this.detailsData = result.data;
        });
    }
}

export default DetailsController;
DetailsController.$inject = ['$stateParams', 'detailsService'];
