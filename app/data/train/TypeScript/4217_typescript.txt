import * as fs from 'fs';
import * as mocha from 'mocha'
import {expect} from 'chai'
import {Validator} from '../src/validator/validator'



describe('Validate Schemas', () => {
    var validator: Validator;
    var _self = this;
    before((done) => {
        _self.validator = new Validator();
        _self.validator.Initialize()
        .then(() => done())
        .catch((err: any) => done(err));
    })
    it('BasicValidation', (done) => {
        var fileContent = fs.readFileSync('./test/baseFiles/validParam.json', 'utf8').toString();
        let paramSchema: any = JSON.parse(fileContent);
        _self.validator.validateSchema('./test/baseFiles/validTemplate.json', paramSchema.parameters)
        .then((result: Error[]) => {
            if(result) expect(result).to.be.empty;
            done();
        })
        .catch((err: any) => done(err));
    });
    it('Invalid Json', () => {
        var errors = _self.validator.validateJson('./test/baseFiles/invalidJson.json');
        expect(errors).to.be.not.undefined;
    });
});