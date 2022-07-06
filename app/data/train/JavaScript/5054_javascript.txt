'use strict';

function UserListController(userList) {
    var vm = this;

    console.log('UserListController');
    vm.userList = userList;
}

module.exports = UserListController;