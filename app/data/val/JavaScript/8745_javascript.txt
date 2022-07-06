import {Ternary} from "./ternary";
import {cast} from "./cast";

export const F = Object.assign(new Ternary("F", false), {
    and() {
        return this;
    },
    xor(b) {
        return cast(b);
    }
});

Ternary.F = F;
