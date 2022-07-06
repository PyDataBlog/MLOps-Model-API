import React, { Component } from 'react';
import CatagoryBox1 from '../CatagoryBox1';
import { BgBox } from './style';

function BackgroundBox (props) {
  const {bcolor} = props;
  const {data} = props;
  const cata = data.map((d,i) => <CatagoryBox1 data={d} key={i} />);
  return (
      <BgBox bcolor={bcolor}>
        {cata}
      </BgBox>
  );
};

export default BackgroundBox;
