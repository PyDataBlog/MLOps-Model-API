import { Component, OnInit, Input } from '@angular/core';
import {Response} from '@angular/http';
import { Food } from "../food.interface";
import { FoodService } from "../food.service";
import { SlickSliderComponent as slickComponent } from "../slick.slider.component";
import * as jQuery from 'jquery';
import 'slick-carousel/slick/slick';
@Component({
  selector: 'app-foods',
  templateUrl: './foods.component.html',
  styleUrls: ['./foods.component.css'],
   interpolation: ["{%", "%}"],
   providers : [slickComponent]
  
})
export class FoodsComponent implements OnInit {
foods: Food[];


constructor(private foodService: FoodService ) {
 
 }

  ngOnInit() {
   this.onGetFoodsByCategorie(5);
    }
  
  onGetFoods(){
this.foodService.getFoods()
.subscribe(
(foods: Food[])=> this.foods = foods,
(error : Response)=> console.log(error)
);
  }
   onGetFoodsByCategorie(id){
this.foodService.getFoodsByCat(id)
.subscribe(
(foods: Food[])=> this.foods = foods,
(error : Response)=> console.log(error)
);
  } 

}
