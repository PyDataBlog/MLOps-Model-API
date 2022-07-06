import * as $ from "jquery";

interface ContentElement {
    getContent(): string;
}

export interface ScrollElement {
    element: ContentElement;
    instance: any;
}

/* Used for a performant list scroll controller. Allows thousands of elements to be scrolled in a list
 * without causing major delays in writing the elements to it */
class Scroller {
    private _elementList: JQuery;
    private _elementScroller: JQuery;
    private _elements: ScrollElement[];
    private _filteredElements: ScrollElement[];
    private _elementHeight: number;
    private _elementsToDisplay: number;
    private _startIndex: number;
    private _endIndex: number;
    private _trueScrollPosition: number;
    private _filter: ((element: ScrollElement) => boolean) | null = null;

    constructor(elementList: JQuery, elementScroller: JQuery, elements: ScrollElement[], elementHeight: number, elementsToDisplay: number = 40) {
        this._elementList = elementList;
        this._elementScroller = elementScroller;
        this._elements = elements;
        this._filteredElements = [];
        this._elementHeight = elementList.children().first().outerHeight();
        this._elementsToDisplay = elementsToDisplay;
        this._startIndex = 0;
        this._endIndex = elementsToDisplay;
        this.displayElements();
    }

    public get elementList(): JQuery {
        return this._elementList;
    }

    public get elements(): ScrollElement[] {
        return this._elements;
    }

    public get elementHeight(): number {
        return this._elementHeight;
    }

    public get elementsToDisplay(): number {
        return this._elementsToDisplay;
    }

    public get startIndex(): number {
        return this._startIndex;
    }

    public get endIndex(): number {
        return this._endIndex;
    }

    public get trueScrollPosition(): number {
        return this._trueScrollPosition;
    }

    public set elements(elements: ScrollElement[]) {
        this._elements = elements;
        this._redraw(this._startIndex, this._endIndex);
    }

    public set scrollTop(scrollTop: number) {
        this._elementScroller[0].scrollTop = scrollTop;
    }

    public moveElementToFront(elementInstance: any, elementId: string): void {
        const filteredIndex = this._filteredElements.findIndex(e => e.instance === elementInstance);

        if (filteredIndex > -1) {
            const element = this._filteredElements.splice(filteredIndex, 1)[0];
            this._filteredElements.unshift(element);
        }

        const unfilteredIndex = this._elements.findIndex(e => e.instance === elementInstance);
        if (unfilteredIndex > -1) {
            const element = this._elements.splice(unfilteredIndex, 1)[0];
            this._elements.unshift(element);
        }

        if ((this._filter !== null && filteredIndex > -1) || (this._filter === null && unfilteredIndex > -1)) {
            const index = this._filter === null ? unfilteredIndex : filteredIndex;
            // Element is in view then it can be moved
            if (this._startIndex <= index && this._endIndex >= index) {
                this._elementList.prepend($(elementId));
            } else {
                // Element is not in the viewable list, so it must be redrawn
                this._redraw(this._startIndex, this._endIndex);
            }
        }
    }

    public reset(): void {
        this._startIndex = 0;
        this._endIndex = this._elementsToDisplay;
        this._elementScroller[0].scrollTop = 0;
    }

    public prependElement(element: ScrollElement): void {
        this._elements.unshift(element);
        this._redraw(this._startIndex, this._endIndex);
    }

    public setFilter(f: (element: ScrollElement) => boolean): void {
        this._filter = f;
        this._filteredElements = this._elements.filter(e => f(e));
        this._redraw(this._startIndex, this._endIndex);
    }

    public removeFilter(): void {
        this._filter = null;
        this._filteredElements = [];
        this._redraw(this._startIndex, this._endIndex);
    }

    public appendElement(element: ScrollElement, redraw: boolean = true): void {
        this._elements.push(element);
        if (redraw) {
            this._redraw(this._startIndex, this._endIndex);
        }
    }

    // Updates the current display of elements in the list element
    private displayElements(): void {
        const elements = this._filter !== null ? this._filteredElements : this._elements;
        this._elementList.empty();

        let elementsString = "";
        let elem: ScrollElement;
        for (let i = this._startIndex; i <= this._endIndex; i++) {
            elem = elements[i];
            if (elem) {
                elementsString += elem.element.getContent();
            }
        }

        this._elementList.append(elementsString);
    }

    // Called when a scroll event is trigger on the list element
    public onScroll(): void {
        const elements = this._filter !== null ? this._filteredElements : this._elements;
        // Find scroll positions
        const scrollPosition: number = this._elementScroller[0].scrollTop;
        const trueScrollPosition: number = this._startIndex * this._elementHeight + scrollPosition;
        const htmlElementScroller = this._elementScroller[0] as HTMLElement;
        const maxScrollPosition: number = htmlElementScroller.scrollHeight - htmlElementScroller.offsetHeight;

        // Set new true scroll position
        this._trueScrollPosition = trueScrollPosition;

        // Calculate start/end indexes for idsplay
        const start = Math.floor((trueScrollPosition / this._elementHeight) - (this._elementsToDisplay / 2));
        const end = Math.floor((trueScrollPosition / this._elementHeight) + (this._elementsToDisplay / 2));

        // Check whether an update to the display is necessary (close to boundaries)
        if (this._startIndex > 0 && start < this._startIndex && scrollPosition < this._elementHeight) {
            this._redraw(start, end);
        } else if (this._endIndex + 1 < elements.length && scrollPosition > maxScrollPosition - this._elementHeight) {
            this._redraw(start, end);
        }
    }

    /**
     * Re-inserts the elements and sets the scroll of the scroll area
     */
    public redraw(): void {
        this._redraw(this._startIndex, this._endIndex);
    }

    /**
     * Updates the display of elements and corrects the scroll position
     */
    private _redraw(start: number, end: number): void {
        if (!this._elementHeight) {
            this._elementHeight = this._elementList.children().first().outerHeight();
        }

        const oldStart = this._startIndex;
        const oldEnd = this._endIndex;

        // Ensure start is no lower than the lowest
        if (start < 0) {
            start = 0;
            end = start + this._elementsToDisplay;
        }

        this._startIndex = start;
        this._endIndex = end;
        this.displayElements();
        this._elementScroller[0].scrollTop = this.trueScrollPosition - ((this._startIndex) * this._elementHeight);
    }
}

export default Scroller;
