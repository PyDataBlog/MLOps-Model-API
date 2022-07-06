# Lesson 6: Interface, Abstract Classes, and Polymorphism in Typescript

## Installation

1. Install typescript `npm install -g typescript` or if you have installed typescript globally already, go to directory and command `npm link typescript`
2. Install webpack globally `npm install -g webpack` if you haven't do so yet
3. Setup *lesson6* as npm project by going to its directory and typing `npm install` to install setup based on *package.json*.

## Deployment/Trying it out

1. In *lesson6* directory, command `webpack` to build *dist/bundle.js* so that *index.html* can access it via src tag as specified
2. Copy path or type file directory of *index.html* in browser preferrably on chrome.

## Interface

In OOP, an *Interface* is a construct that can be implemented by a class. Through *Interfaces* we 
are able to create a variable which type and assign different instance of classes implementing that interface. Typescript being a superset of Javascript for ES6, 
supports this feature in OOP by creating a file containing the following code:

    interface Implementable {/*some code*}

and can be implemented by a class as follows:

    class MyClass implements Implementable {}

Interfaces also allow us to enforce instructions that must be implemented by the implementing classes. In Typescript
both the property and methods can be specified as implementables (should be implemented) by the implementing class whereas in some 
language such as *Java* methods are the only members that can be made implementable. 

    interface Shape {
        name:string
        draw2D: (canvas:CanvasRenderingContext2D) => void;
    }

    class Circle implements Shape {
        public name: string;
        public draw2D = (canvas:CanvasRenderingContext2D): void => {
            //2D implementation
        }
    }

    //Rectangle will have transpilation error since name is not specified as required by Shape interface
    class Rectangle implements Shape {
        public draw2D = (canvas:CanvasRenderingContext2D): void => {
            //2D implementation
        }
    }

Interfaces can also be used for creating proper format for your functions that have multiple inputs. So the following

    interface Config {
        configOne: string;
        configTwo: number;
        configThree: boolean;
    }

    function setup = (appConfig:Config) => {
        //setup code
    }

    setup({configOne: "aString", configTwo: 9, configThree: true})

is easier to read and/or code than this

    function setup = (configOne:string, configTwo: number, configThree: boolean) => {
        //setup code
    }

    setup("aString", 9, true)

One of the downsides with *Interface* on strongly typed languages is that they result to boiler plate code
since the implementing class should always include the required methods to be implemented in the source code. Consider the following

    interface Shape {
        getName: () => string;
        draw2D: (context: CanvasRenderingContext2D) => void;
    }

    class Circle implements Shape {
        getName = ():string => {
            return "Circle"
        }

        draw2D = (context: CanvasRenderingContext2D):void => {
            //circle implementation
        }
    }

    class IrregularShape implements Shape {
        getName = ():string => {
            return ""; // getName still needs to be included in the source code resulting to boiler plate code
        }

        draw2D = (context: CanvasRenderingContext2D):void => {
            //irregular shape implementation
        }
    }

Good thing in Typescript *Interfaces* can have optional implementable members. To specify that it is optional we use the notation `?:` instead of `:`.
So the following can be done to remove boiler plate on Irregular Shape:

    interface Shape {
        getName?: () => string;
        draw2D: (context: CanvasRenderingContext2D) => void;
    }

    //will transpile
    class IrregularShape implements Shape {
        draw2D = (context: CanvasRenderingContext2D):void => {
            //irregular shape implementation
        }
    }


## Polymorphism

To visualize this, kindly run (see Deployment) our lesson6 project if you haven't do so yet.
As you can see the *draw shape* button just executes `Shape.draw2D()` but each implementation of *Shape*
has there own implementation of `draw2D`. The shape variable being referenced by the draw button takes different
form depending on the selected value in the Shape implementation list. Thus, called `Polymorphism`.

At its basic form, the *Interface and Polymorphsim in Typescript* is coded as follows:

    interface Shape {
        getName?: () => string;
        draw2D: (context: CanvasRenderingContext2D) => void;
    }

    class Circle implements Shape {
        public getName = (): string => {
            return "Circle"
        }
        
        draw2D = (context: CanvasRenderingContext2D):void => {
            //irregular shape implementation
        }
    }

    //other class codes for Triangle, Rectangle, and Irregular shape

    class ShapeRegistry {
        public static getShape = (name:string):Shape => {
            let returnValue: Shape;
            switch (name){
                case 'Circle': returnValue = new Circle(); break;
                //other case handlers for Triangle, Rectangle, and Irregular shape
            }
            return returnValue;
        }
    }

    let canvas; // if you are here to just learn typescript don't bother yourself with canvas but if you want you can 
                // check config.js under src/interfacelesson
    let shape: Shape;
    let dom: Document = document;

    //selectImpl is the id of select/drop down form
    dom.getElementId('selectImpl').addEventListener('change',(selected) => {
        shape = ShapeRegistry.getShape(selected.target.value)
    })

    //invokeButton is the id of select/drop down form
    dom.getElementId('invokeButton').addEventListener('click',() => {
        shape.draw(canvas.getContext('2d'))
    })