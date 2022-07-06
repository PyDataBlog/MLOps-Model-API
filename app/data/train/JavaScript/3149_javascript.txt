import React from 'react';
import Status from 'components/Status';
import renderer from 'react-test-renderer';

describe('Status component', () => {
  function getComponent(piecesLeftCount) {
    return renderer.create(
      <Status piecesLeftCount={piecesLeftCount} />
    );
  }

  it('should show pieces left', () => {
    expect(getComponent(9).toJSON()).toMatchSnapshot();
  });

  it('should show "Done" when no pieces left', () => {
    expect(getComponent(0).toJSON()).toMatchSnapshot();
  });
});
