import {expect} from 'chai';
import {Controller} from '../../src/component/reclamos.component.ts';


describe("Controller Test", () => {

    it("getTitle must return hello plus prestacionName", () => {
        let controller = new Controller();
        controller.solicitudesSimilares = [ {"nombrePrestacion": "aceras", "rubro": "rubro", "refuerzos": 5} ];

        expect(controller.getTitle()).to.equal("hello -- aceras");
    });
});