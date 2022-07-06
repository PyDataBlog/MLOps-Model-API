/// <reference path="../../../typings/main/ambient/TweenLite/TweenLite.d.ts" />
/// <reference path="../../../typings/main/ambient/TimelineMax/TimelineMax.d.ts" />
/// <reference path="../../../typings/main/ambient/jquery/index.d.ts" />
import * as $ from 'jquery';
import * as TimelineMax from 'TimelineMax';
import 'CSSPlugin';
import 'EasePack';
import 'DrawSVGPlugin';
import 'AttrPlugin';

//region Element Selectors
const $container = $('.container');
const $loader = $('#loader');
const $circleL = $('#circleL');
const $circleR = $('#circleR');
const $jump = $('#jump');

const $jumpRef = $jump.clone();
$loader.append($jumpRef);

//region Animation Config
const duration = 0.5;
const infiniteRepeatLoop = -1;
//endregion



//region Timeline Setup

const jumpTl = new TimelineMax({ repeat: infiniteRepeatLoop });

jumpTl
    .set([$container, $loader], {
        position: 'absolute',
        top: '50%',
        left: '50%',
        xPercent: -50,
        yPercent: -50
    })
    .set($jumpRef, {
        transformOrigin: '50% 110%',
        scaleY: -1,
        alpha: 0.05
    })
    .set([$jump, $jumpRef], {
        drawSVG: '0% 0%'
    })
    .set([$circleL, $circleR], {
        attr: {
            rx: 0,
            ry: 0
        }
    })
    .to([$jump, $jumpRef], 0.4, {
        drawSVG: '0% 30%',
        ease: Linear.easeNone
    })
    .to($circleL, 2, {
        attr: {
            rx: '+=30',
            ry: '+=10'
        },
        alpha: 0,
        ease: Power1.easeOut
    }, '-=0.1')
    .to([$jump, $jumpRef], 1, {
        drawSVG: '50% 80%',
        ease: Linear.easeNone
    }, '-=1.9')
    .to([$jump, $jumpRef], 0.7, {
        drawSVG: '100% 100%',
        ease: Linear.easeNone
    }, '-=0.9')
    .to($circleR, 2, {
        attr: {
            rx: '+=30',
            ry: '+=10'
        },
        alpha: 0,
        ease: Power1.easeOut
    }, '-=0.5')
    .timeScale(3)
;

//endregion
