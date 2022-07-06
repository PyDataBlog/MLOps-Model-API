import config from './config';

describe('config', function(){

  it('should exist', function(){
    expect(config).to.be.an('object');
  });

  it('should contain the required keys', function(){
    expect(config.ngxDirective).to.be.an('object');
    expect(config.ngxDirective.name).to.be.a('string');
  });

});
