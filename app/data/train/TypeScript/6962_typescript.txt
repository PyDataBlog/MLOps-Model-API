import {testFunction} from "./app";

describe("The test function", () =>{
    it("should return true if number is bigger than zero", () =>{
        expect(testFunction(5)).toBeTruthy();
    });
});