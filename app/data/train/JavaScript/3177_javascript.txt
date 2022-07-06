window.onload=function() {
    var start = document.getElementById('start');
    start.onclick = function () {
        var name = document.getElementById('Name').value;
        var id = document.getElementById('Id').value;
        var tel = document.getElementById("Tel").value;
            if (name == "") {
            alert("请输入名字");
            return false;
        }
        if (!isName(name)) {
            alert("请输入正确的姓名")
            return false;
        }
        if (id =="") {
            alert("请输入学号");
            return false;
        }
        if (!isId(id)) {
            alert("请输入正确学号");
            return false;
        }
        if (tel == "") {
            alert("请输入电话号码");
            return false;
        }
        if (!isTelephone(tel)) {
            alert("请输入正确的手机号码");
            return false;
        }

        else {
            start.submit();
            // document.getElementById("myform").submit();
            // window.open("answer.html","_self");
        }
    }

    function isName(obj) {
        var nameReg = /[\u4E00-\u9FA5]+$/;
        return nameReg.test(obj);
    }

    function isId(obj) {
        var emailReg = /^2017\d{8}$/;
        return emailReg.test(obj);
    }

    function isTelephone(obj) {
        reg = /^1[34578]\d{9}$/;
        return reg.test(obj);
    }
}