/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.hoang.fu;

/**
 *
 * @author hoangpt
 */
public class Teacher extends Employee implements ITeacher {

  Teacher(String name) {
    this.name = name;
  }
  
  @Override
  float calculateSalary(){
    return 500f;
  }

  @Override
  public int calculateBonus() {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }

  @Override
  public float calculateAllowance() {
    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
  }
  
}
