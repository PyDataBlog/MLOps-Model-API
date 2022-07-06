
import { Directive, ElementRef ,OnInit} from "@angular/core";
@Directive({
    selector: '[scoller]',
})
export class FoodEnteryDirective implements OnInit{
   
    private el: ElementRef;
    
    constructor(el: ElementRef) {
      this.el=el;
     

    }
   ngOnInit(){
      
       var el=this.el;
       //这两个this不是同一个
        el.nativeElement.addEventListener("touchstart", function (event: TouchEvent) {
            this.startX = event.changedTouches["0"].pageX;
            this.startY = event.changedTouches["0"].pageY;
            this.movestart = this.startX;
            this.clientX = event.view.innerWidth;
        });
        el.nativeElement.addEventListener("touchend", function (event: TouchEvent) {
           
            var distanceX = event.changedTouches["0"].pageX - this.startX;
            let elActive: any;
            let elNotActive: any;
            let indicatorP: any;
            let indicatorActive: any;
            let indicatorNotActive: any;
            for (var i = 0; i < el.nativeElement.childNodes.length; i++) {
                if (el.nativeElement.childNodes[i].className == 'food_items items_active') {
                    elActive = el.nativeElement.childNodes[i];
                }
                if (el.nativeElement.childNodes[i].className == 'food_items') {
                    elNotActive = el.nativeElement.childNodes[i];
                }
                if (el.nativeElement.childNodes[i].className == 'food_items') {
                    elNotActive = el.nativeElement.childNodes[i];
                }

                if (el.nativeElement.childNodes[i].className == 'mint-swipe-indicators') {
                    indicatorP = el.nativeElement.childNodes[i];
                    for (var j = 0; j < indicatorP.childNodes.length; j++) {
                        if (indicatorP.childNodes[j].className == 'mint-swipe-indicator is-active') {
                            indicatorActive = indicatorP.childNodes[j];
                            console.log("indicatorActive:" + indicatorActive);
                        }
                        if (indicatorP.childNodes[j].className == 'mint-swipe-indicator') {
                            indicatorNotActive = indicatorP.childNodes[j];
                            console.log("indicatorNotActive:" + indicatorNotActive);
                        }
                    }

                }

            }
            if (Math.abs(distanceX) >= (this.clientX / 2)) {
                elActive.className = '';
                elActive.setAttribute('class', 'food_items');
                elNotActive.setAttribute('class', 'food_items items_active');
                elNotActive.style.transform = 'translateX(0px)';
                elActive.style.transform = 'translateX(0px)';
                indicatorNotActive.setAttribute('class', '');
                indicatorNotActive.setAttribute('class', 'mint-swipe-indicator is-active');
                indicatorActive.setAttribute('class', '');
                indicatorActive.setAttribute('class', 'mint-swipe-indicator');
            }
            if (Math.abs(distanceX) < (this.clientX / 2)) {
                elNotActive.style.transform = 'translateX(0px)'
                elActive.style.transform = 'translateX(0px)';

            }
        });

        el.nativeElement.addEventListener("touchmove", function (event: TouchEvent) {
            //判断滑动方向
            
            var distanceX = event.changedTouches["0"].pageX - this.startX;
            if (event.changedTouches["0"].pageX > this.movestart) {
                for (var i = 0; i < el.nativeElement.childNodes.length; i++) {
                    if (el.nativeElement.childNodes[i].className == 'food_items items_active') {
                        el.nativeElement.childNodes[i].style.transform = 'translateX(' + distanceX + 'px)';
                        // console.log("move");

                    }
                    if (el.nativeElement.childNodes[i].className == 'food_items') {
                        el.nativeElement.childNodes[i].style.transform = 'translateX(-' + (this.clientX - distanceX) + 'px)';
                    }

                    // console.log("->distanceX", distanceX);
                }
                // console.log("右滑");
                this.movestart = event.changedTouches["0"].pageX;
            }
            if (event.changedTouches["0"].pageX < this.movestart) {
                for (var i = 0; i < el.nativeElement.childNodes.length; i++) {
                    if (el.nativeElement.childNodes[i].className == 'food_items items_active') {
                        el.nativeElement.childNodes[i].style.transform = 'translateX(' + distanceX + 'px)';

                    }
                    if (el.nativeElement.childNodes[i].className == 'food_items') {
                        if (-(-this.clientX + distanceX) >= this.clientX) {
                            el.nativeElement.childNodes[i].style.transform = 'translateX(' + (this.clientX + distanceX) + 'px)';
                        } else {
                            el.nativeElement.childNodes[i].style.transform = 'translateX(' + (-this.clientX + distanceX) + 'px)';
                        }

                    }

                    // console.log("<-distanceX", distanceX);
                }
                //console.log("左滑");
                this.movestart = event.changedTouches["0"].pageX;
            }


        }, false);
   }

}