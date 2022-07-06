import { Directive, ElementRef, Input } from '@angular/core';

@Directive({ selector: '[umeScroll]' })
export class ScrollDirective {
	constructor(el: ElementRef) {
		const element = el.nativeElement;

		element.addEventListener('mouseenter', () => {
			element.classList.add('scroll');
		});

		element.addEventListener('mouseleave', () => {
			element.classList.remove('scroll');
		});
	}
}
