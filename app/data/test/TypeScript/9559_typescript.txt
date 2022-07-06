import { shallow, ShallowWrapper } from "enzyme";
import * as React from "react";
import Timer from "../Timer";

jest.mock("../../../Leaderboard/container");

const testProps: any = {
  secondsRemaining: 10,
};

describe("<Timer />", () => {
  let wrapper: ShallowWrapper<any, any>;
  beforeEach(() => {
    wrapper = shallow(<Timer {...testProps} />);
  });
  it("renders", () => {
    expect(wrapper.render()).toMatchSnapshot();
  });
  it("state is in good shape", () => {
    expect(wrapper.instance().state.secondsRemaining).toEqual(10);
    expect(wrapper.instance().state.color).toEqual("#FF530D");
    expect(wrapper.instance().state.fontSize).toEqual(60);
  });
  it("state tick updates state", () => {
    (wrapper.instance() as any).tick();
    expect(wrapper.instance().state.secondsRemaining).toEqual(9);
  });
});
