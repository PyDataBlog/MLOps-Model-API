var Turtle = function () {
    this.d = 0;
    this.x = 0;
    this.y = 0;
    this.rounding = false;
	this.invert = false;
}

Turtle.prototype.setX = function (val) {
    this.x = val;
    return this;
}

Turtle.prototype.setY = function (val) {
    this.y = val;
    return this;
}

Turtle.prototype.setDegree = function (deg) {
    this.d = deg;
    return this;
}

Turtle.prototype.setInvert = function (bool) {
	this.invert = bool;
	return this;
}

Turtle.prototype.setRounding = function (bool) {
    this.rounding = bool;
    return this;
}

Turtle.prototype.rt = function (degrees) {
    if (this.invert) {
	    this.d += degrees;
	} else {
	    this.d -= degrees;
	    this.d += 360; // to ensure that the number is positive
	}
    this.d %= 360;
    return this;
}

Turtle.prototype.lt = function (degrees) {
    if (this.invert) {
	    this.d -= degrees;
	    this.d += 360; // to ensure that the number is positive
	} else {
	    this.d += degrees;
	}
    this.d %= 360;
    return this;
}
Turtle.prototype.adj = function (degrees, hyp) {
    var adj = Math.cos(degrees * Math.PI / 180) * hyp;
    if (this.rounding) {
        return Math.round(adj);
    } else {
        return adj;
    }
}

Turtle.prototype.opp = function (degrees, hyp) {
    var opp = Math.sin(degrees * Math.PI / 180) * hyp;
    if (this.rounding) {
        return Math.round(opp);
    } else {
        return opp;
    }
}

Turtle.prototype.fd = function (magnitude) {
    if (this.d < 90) {
        // x == adjacent
        this.x += this.adj(this.d, magnitude);
        // y == opposite
        this.y += this.opp(this.d, magnitude);
    } else if (this.d < 180) {
        // x == -opposite
        this.x -= this.opp(this.d - 90, magnitude);
        // y == adjacent
        this.y += this.adj(this.d - 90, magnitude);
    } else if (this.d < 270) {
        // x == -adjacent
        this.x -= this.adj(this.d - 180, magnitude);
        // y == -opposite
        this.y -= this.opp(this.d - 180, magnitude);
    } else if (this.d < 360) {
        // x == opposite
        this.x += this.opp(this.d - 270, magnitude);
        // y == -adjacent
        this.y -= this.adj(this.d - 270, magnitude);
    }
    return this;
}

Turtle.prototype.bk = function (magnitude) {
    if (this.d < 90) {
        // x -= adjacent
        this.x -= this.adj(this.d, magnitude);
        // y -= opposite
        this.y -= this.opp(this.d, magnitude);
    } else if (this.d < 180) {
        // x == +opposite
        this.x += this.opp(this.d - 90, magnitude);
        // y == -adjacent
        this.y -= this.adj(this.d - 90, magnitude);
    } else if (this.d < 270) {
        // x == opposite
        this.x += this.adj(this.d - 180, magnitude);
        // y == adjacent
        this.y += this.opp(this.d - 180, magnitude);
    } else if (this.d < 360) {
        // x == -opposite
        this.x -= this.opp(this.d - 270, magnitude);
        // y == adjacent
        this.y += this.adj(this.d - 270, magnitude);
    }
    return this;
}