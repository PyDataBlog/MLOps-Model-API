(function() {
  'use strict';
  
  angular.module('character-tracker.charactersheet')
  .controller('GearController', GearController);
  
  GearController.$inject =['GearService', 'InventoryService', '$scope'];
  
  function GearController(GearService, InventoryService, $scope) {
    var vm = this;
    var currentItem = '';
    
    vm.inventory = InventoryService.getItems();
    
    vm.gearSlots = GearService.getGearSlots();
    
    vm.equipItem = function (item, slot){
      if (item.equipped) {
        vm.unequipItem(item);
      }
      
      for (i = 0; i < vm.gearSlots.length; i++) {
        if (vm.gearSlots[i].slot === slot) {
          vm.gearSlots[i].equipped = true;
          vm.gearSlots[i].equippedItem = item;
        }
      }
      InventoryService.equipItem(item);
    }
    
    vm.unequipItem = function(item) {
      for (i = 0; i < vm.gearSlots.length; i++) {
        if (vm.gearSlots[i].equippedItem == item) {
          vm.gearSlots[i].equippedItem = {};
        }
      InventoryService.unequipItem(item);
      }  
    }
    
    vm.getItemAtSlot = function(slot) {
      currentItem = InventoryService.getItemAtSlot(slot);
    }
    
    init();
    
    function init() {
      $.material.init();
    }
    
    return vm;
  }
}());