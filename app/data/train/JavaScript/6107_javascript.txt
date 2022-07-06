var library = require('./library.js');

var check_cond = function(num, div, start)
{
    var n = '';

    for(var i = start; i < start + 3; i++)
    {
        n = n + num.toString().charAt(i - 1);
    }

    if(parseInt(n) % div === 0)
    {
        return true;
    }

    return false;
}

var check_all = function(num)
{
    var all = [2, 3, 5, 7, 11, 13, 17];

    for(var i = 0; i < all.length; i += 1)
    {
        if(!check_cond(num, all[i], i + 2))
        {
            return false;
        }
    }

    return true;
}

var solve = function ()
{
    var sum   = 0;
    var start = 1234567890;
    var end   = 9876543210;

    for(var i = start, count = 0; i <= end; i += 1, count += 1)
    {
        if(count % 1000000 == 0)
        {
            console.log("\$i : " + i);
        }

        if(!library.is_pandigital(i, 0))
        {
            continue;
        }

        if(!check_all(i))
        {
            continue;
        }

        console.log("OK : " + i);

        sum += i;
    }
};

var check_all_2 = function(num)
{
    var y = num.toString();
    var n = [0];

    for(var i = 0; i < y.length; i += 1)
    {
        n.push(parseInt(y[i]));
    }

    if(n[4] % 2 != 0)
    {
        return false;
    }

    var a = n[3] + n[4] + n[5];

    if(a % 3 != 0)
    {
        return false;
    }

    if(n[6] % 5 != 0)
    {
        return false;
    }

    var b = n[5] * 10 + n[6] - 2 * n[7];

    if(b % 7 != 0)
    {
        return false;
    }

    var c = n[6] * 10 + n[7] - n[8];

    if(c % 11 != 0)
    {
        return false;
    }

    var d = n[7] * 10 + n[8] + 4 * n[9];

    if(d % 13 != 0)
    {
        return false;
    }

    var e = n[8] * 10 + n[9] - 5 * n[10];

    if(e % 17 != 0)
    {
        return false;
    }

    return true;

}

var solve_2 = function ()
{
    var sum   = 0;
    var start = 1234567890;
    var end   = 9876543210;

    for(var i = start, count = 0; i <= end; i += 1, count += 1)
    {
        if(count % 1000000 == 0)
        {
            console.log("\$i : " + i);
        }

        if(!check_all_2(i))
        {
            continue;
        }

        if(!library.is_pandigital_v2(i, 0))
        {
            continue;
        }

        console.log("OK : " + i);
        sum += i;
    }
};

var sum = solve_2();
console.log(sum);

//var num = process.argv[2];
//console.log(check_all_2(num));
